/// Generates a cartesian product of test cases from a test function with parameters.
///
/// The function must have at least one parameter, and
/// every parameter must be annotated with at least one `#[case(NAME = VALUE)]` attribute.
/// The `NAME` is the identifier used for this case in the generated test names.
///
/// # Example
///
/// ```
/// #![feature(macro_attr)]
/// # use all_is_cubes_base::cartesian_product_test;
/// #[cartesian_product_test]
/// fn my_test(
///     #[case(opaque = false)]
///     #[case(transparent = true)]
///     transparent: bool,
///     #[case(still = false)]
///     #[case(animated = true)]
///     animated: bool,
/// ) {
///     // do something different based on `transparent` and `animated`
/// }
/// ```
///
/// This will generate a module named `my_test` (the same as the test function)
/// and test functions named
/// `my_test::opaque::still`,
/// `my_test::opaque::animated`,
/// `my_test::transparent::still`, and
/// `my_test::transparent::animated`.
#[macro_export]
macro_rules! cartesian_product_test {
    attr() (
        $(#[doc = $doc:expr])*
        $(#[test])?
        $(#[ignore = $ignore_reason:expr])?
        fn $test_name:ident (
            $(
                $( #[case($case_name:ident = $case_value:expr)] )+
                $parameter:ident : $param_ty:ty
            ),* $(,)?
        ) $body:block
    ) => {
        $crate::cartesian_product_test_helpers!(
            @undecorate_args
            $test_name,
            (
                $(
                    $(#[case($case_name = $case_value)])+ $parameter : $param_ty
                ),*
            )
            $body
        );

        #[cfg(test)]
        $(#[doc = $doc])*
        mod $test_name {
            use super::*;
            $crate::cartesian_product_test_helpers!(
                @recurse_over_params
                ($(#[ignore = $ignore_reason])?),
                $test_name,
                (),
                (
                    $(
                        $(#[case($case_name = $case_value)])+ $parameter : $param_ty
                    ),*
                )
                $body
            );
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! cartesian_product_test_helpers {
    // Just generates a function that doesn’t have the `#[case]` attributes.
    (
        @undecorate_args
        $test_name:ident,
        (
            $(
                $(#[case($case_name:ident = $case_value:expr)])+ $parameter:ident : $param_ty:ty
            ),* $(,)?
        )
        $body:block
    ) => {
        #[cfg(test)]
        fn $test_name ($( $parameter : $param_ty ),*) $body
    };

    // Recursive case of expansion: pop off the first parameter (and group the remaining params)
    (
        @recurse_over_params
        $test_attributes_group:tt,
        $test_name:ident,
        $preceding_case_values_group:tt,
        (
            $(#[case($case_name:ident = $case_value:expr)])+
            $parameter:ident : $param_ty:ty,
            $($remaining_params:tt)+
        )
        $body:block
    ) => {

        $crate::cartesian_product_test_helpers!(
            @pop_one_param
            $test_attributes_group,
            $test_name,
            [$(# $preceding_case_values_group, [case($case_name = $case_value)])+]
            $parameter : $param_ty,
            ($($remaining_params)+)
            $body
        );
    };
    // Helper for above
    (
        @pop_one_param
        $test_attributes_group:tt,
        $test_name:ident,
        [$(# ($($preceding_case_values_tok:tt)*), [case($case_name:ident = $case_value:expr)])+]
        $parameter:ident : $param_ty:ty,
        $remaining_params_group:tt
        $body:block
    ) => {
        $(
            mod $case_name {
                use super::*;
                $crate::cartesian_product_test_helpers!(
                    @recurse_over_params
                    $test_attributes_group,
                    $test_name,
                    ( $($preceding_case_values_tok)* $case_value,),
                    $remaining_params_group
                    $body
                );
            }
        )*
    };

    // Base case: we have exactly one parameter to expand cases of.
    // Generate functions, not modules.
    (
        @recurse_over_params
        $test_attributes_group:tt,
        $test_name:ident,
        $preceding_case_values_group:tt,
        (
            $( #[case($case_name:ident = $case_value:expr)])+
            $parameter:ident : $param_ty:ty
            $(,)?
        ) $body:block
    ) => {
        $(
            $crate::cartesian_product_test_helpers!(
                @finish_one_case
                $test_attributes_group,
                $test_name,
                $preceding_case_values_group
                ($case_name = $case_value)
                $body
            );
        )*
    };
    // Helper for above. Generate one function, concatenating its argument list.
    (
        @finish_one_case
        ($($test_attributes_tok:tt)*),
        $test_name:ident,
        ($($preceding_case_values_tok:tt)*)
        (
            $case_name:ident = $case_value:expr
        ) $body:block
    ) => {

        #[test]
        $($test_attributes_tok)*
        fn $case_name() {
            $test_name( $($preceding_case_values_tok)* $case_value)
        }
    };
}
