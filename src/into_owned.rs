use std::borrow::Cow;

pub trait IntoOwned {
    type Owned;

    fn into_owned(self) -> Self::Owned;
}

impl<B> IntoOwned for Cow<'_, B>
where
    B: ToOwned + ?Sized,
{
    type Owned = <B as ToOwned>::Owned;

    fn into_owned(self) -> <Self as IntoOwned>::Owned {
        self.into_owned()
    }
}

impl<T: IntoOwned> IntoOwned for Option<T> {
    type Owned = Option<T::Owned>;

    fn into_owned(self) -> Self::Owned {
        self.map(IntoOwned::into_owned)
    }
}

impl<T: IntoOwned> IntoOwned for Vec<T> {
    type Owned = Vec<T::Owned>;

    fn into_owned(self) -> Self::Owned {
        self.into_iter().map(IntoOwned::into_owned).collect()
    }
}
