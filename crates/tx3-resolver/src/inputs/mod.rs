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

#[cfg(test)]
mod tests;

pub use canonical::{required_collateral, CanonicalQuery};

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

        // What the ledger will demand of the collateral inputs for the fee this
        // pass is resolving against. The TIR only carries what the `.tx3`
        // source declared — typically `min_amount: fees`, which is a full
        // `collateralPercentage - 100` short of what the ledger accepts.
        let min_collateral = required_collateral(self.fees, self.collateral_percentage);

        for (name, query) in tx3_tir::reduce::find_queries(&tx) {
            let mut query = CanonicalQuery::try_from(query)?;

            if query.collateral {
                query.raise_lovelace_floor(min_collateral);
            }

            queries.push((name, query));
        }

        self.set_input_queries(queries);
        self.resolve_queries(utxos).await?;

        let all_inputs = self.to_input_map();
        let out = tx3_tir::reduce::apply_inputs(tx, &all_inputs)?;

        Ok(out)
    }
}
