pub mod compile;
pub mod encoding;
pub mod model;
pub mod reduce;

pub trait Visitor {
    fn reduce(
        &mut self,
        expr: model::v1beta0::Expression,
    ) -> Result<model::v1beta0::Expression, crate::reduce::Error>;
}

pub trait Node: Sized {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::reduce::Error>;
}
