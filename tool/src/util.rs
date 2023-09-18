use core::hash::Hash;

use diplomat_core::{ast, Env};

pub fn get_all_custom_types(env: &Env) -> SetOfAstTypes<&ast::CustomType> {
    let mut all_types = SetOfAstTypes::default();

    for (path, _name, symbol) in env.iter_items() {
        if let ast::ModSymbol::CustomType(c) = symbol {
            all_types.insert((path.clone(), c));
        }
    }

    all_types
}

type AstElement<T> = (ast::Path, T);

/// Ordered set of AST types for deterministic traversal
pub struct SetOfAstTypes<T> {
    set: std::collections::HashSet<AstElement<T>>,
    order: Vec<AstElement<T>>,
}

impl<T> Default for SetOfAstTypes<T> {
    fn default() -> Self {
        Self {
            set: std::collections::HashSet::new(),
            order: Vec::new(),
        }
    }
}

impl<T: Eq + Hash> SetOfAstTypes<T> {
    pub fn sort_by_key<K, F>(&mut self, f: F)
    where
        F: FnMut(&AstElement<T>) -> K,
        K: Ord,
    {
        self.order.sort_by_key(f)
    }

    pub fn contains(&self, elem: &AstElement<T>) -> bool {
        self.set.contains(elem)
    }

    pub fn insert(&mut self, elem: AstElement<T>)
    where
        T: Clone,
    {
        self.set.insert(elem.clone());
        self.order.push(elem);
    }
}

impl<'a, T> IntoIterator for &'a SetOfAstTypes<T> {
    type Item = &'a AstElement<T>;

    type IntoIter = std::slice::Iter<'a, AstElement<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.order.iter()
    }
}

impl<T> IntoIterator for SetOfAstTypes<T> {
    type Item = AstElement<T>;

    type IntoIter = std::vec::IntoIter<AstElement<T>>;

    fn into_iter(self) -> Self::IntoIter {
        self.order.into_iter()
    }
}
