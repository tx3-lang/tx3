//! Tx input resolution pipeline.
//!
//! Orchestrates three stages:
//! 1. **Narrow**: query the UTxO store to build a pool of candidate UTxOs
//! 2. **Approximate**: filter and rank candidates for each query independently
//! 3. **Assign**: allocate UTxOs across all queries simultaneously

use tx3_tir::encoding::AnyTir;

use crate::job::ResolveJob;
use crate::{Error, UtxoStore};

mod approximate;
pub(crate) mod assign;
mod canonical;
mod narrow;
mod order;

#[cfg(test)]
mod tests;

pub use canonical::CanonicalQuery;

impl ResolveJob {
    /// Run the full input resolution pipeline: narrow, approximate, assign.
    pub async fn resolve_queries<T: UtxoStore>(&mut self, utxos: &T) -> Result<(), Error> {
        self.build_utxo_pool(utxos).await?;
        self.approximate_queries();
        self.assign_all()?;

        Ok(())
    }

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

        self.set_input_queries(queries);
        self.resolve_queries(utxos).await?;

        let all_inputs = self.to_input_map();
        let out = tx3_tir::reduce::apply_inputs(tx, &all_inputs)?;

        Ok(out)
    }
}
