//! Tx input resolution pipeline.
//!
//! Orchestrates three stages:
//! 1. **Narrow**: query the UTxO store to build a pool of candidate UTxOs
//! 2. **Approximate**: filter and rank candidates for each query independently
//! 3. **Assign**: allocate UTxOs across all queries simultaneously

use std::collections::BTreeMap;

use tx3_tir::encoding::AnyTir;
use tx3_tir::model::core::{UtxoRef, UtxoSet};

use crate::job::{InputResolutionJob, ResolveJob};
use crate::{Error, UtxoStore};

mod approximate;
pub(crate) mod assign;
mod canonical;
mod narrow;

#[cfg(test)]
pub(crate) mod test_utils;
#[cfg(test)]
mod tests;

pub use canonical::CanonicalQuery;

impl InputResolutionJob {
    /// Run the full input resolution pipeline: narrow, approximate, assign.
    pub async fn resolve_queries<T: UtxoStore>(
        &mut self,
        utxos: &T,
    ) -> Result<BTreeMap<String, UtxoSet>, Error> {
        // 1. Narrow: build pool of candidate UTxOs from all queries
        self.build_utxo_pool(utxos).await?;

        // 2. Approximate: rank candidates for each query independently
        self.approximate_queries();

        // 3. Assign: allocate UTxOs across all queries
        self.assign_all();

        // 4. Validate: ensure all queries were resolved
        let pool_refs: Vec<UtxoRef> = self
            .pool
            .as_ref()
            .map(|p| p.keys().cloned().collect())
            .unwrap_or_default();

        let mut all_inputs = BTreeMap::new();

        for qr in &self.queries {
            let selection = qr.selection.as_ref().expect("selection must be set");
            if selection.is_empty() {
                return Err(Error::InputNotResolved(
                    qr.name.clone(),
                    qr.query.clone(),
                    pool_refs,
                ));
            }
            all_inputs.insert(qr.name.clone(), selection.clone());
        }

        Ok(all_inputs)
    }
}

impl ResolveJob {
    /// Resolve all input queries in a TIR transaction.
    pub async fn resolve_inputs<T: UtxoStore>(
        &mut self,
        tx: AnyTir,
        utxos: &T,
    ) -> Result<AnyTir, Error> {
        let mut queries: Vec<(String, CanonicalQuery)> = Vec::new();

        for (name, query) in tx3_tir::reduce::find_queries(&tx) {
            queries.push((name, CanonicalQuery::try_from(query)?));
        }

        let mut irj = InputResolutionJob::new(queries);
        let all_inputs = irj.resolve_queries(utxos).await?;
        self.input_resolution = Some(irj);

        let out = tx3_tir::reduce::apply_inputs(tx, &all_inputs)?;

        Ok(out)
    }
}
