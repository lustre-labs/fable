import gleam/dict.{type Dict}
import gleam/dynamic/decode.{type Decoder}
import gleam/option.{type Option}

// --- TYPES -------------------------------------------------------------------

/// A Gleam package.
///
pub type Package {
  Package(
    name: String,
    version: String,
    /// The Gleam version constraint that the package specifies in its
    /// `gleam.toml`.
    gleam_version_constraint: Option(String),
    modules: Dict(String, Module),
  )
}

/// A Gleam module.
///
pub type Module {
  Module(
    /// All the lines composing the module's documentation (that is every line
    /// preceded by a `////`).
    documentation: List(String),
    /// The public type aliases defined in the module.
    type_aliases: Dict(String, TypeAlias),
    /// The public custom types defined in the module.
    types: Dict(String, TypeDefinition),
    /// The public constants defined in the module.
    constants: Dict(String, Constant),
    /// The public functions defined in the module.
    functions: Dict(String, Function),
  )
}

/// A Gleam type alias.
///
/// ```gleam
/// // This is a type alias.
/// type Ints = List(Int)
/// ```
///
pub type TypeAlias {
  TypeAlias(
    /// The type alias' documentation comment (that is every line preceded by
    /// `///`).
    ///
    documentation: Option(String),
    /// If the type alias is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    /// The number of type variables of the type alias.
    ///
    /// ```gleam
    /// type Results(a, b) = List(Result(a, b))
    /// //   ^^^^^^^^^^^^^ This type alias has 2 type variables.
    ///
    /// type Ints = List(Int)
    /// //   ^^^^ This type alias has 0 type variables.
    /// ```
    ///
    parameters: Int,
    /// The aliased type.
    ///
    /// ```gleam
    /// type Ints = List(Int)
    /// //          ^^^^^^^^^ This is the aliased type.
    /// ```
    ///
    alias: Type,
  )
}

/// A Gleam custom type definition.
///
/// ```gleam
/// // This is a custom type definition.
/// pub type Result(a, b) {
///   Ok(a)
///   Error(b)
/// }
/// ```
///
pub type TypeDefinition {
  TypeDefinition(
    /// The type definition's documentation comment (that is every line preceded
    /// by `///`).
    ///
    documentation: Option(String),
    /// If the type definition is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    /// The number of type variables of the type definition.
    ///
    /// ```gleam
    /// type Result(a, b) { ... }
    /// //   ^^^^^^^^^^^^ This type definition has 2 type variables.
    ///
    /// type Person { ... }
    /// //   ^^^^^^ This type alias has 0 type variables.
    /// ```
    ///
    parameters: Int,
    /// The type constructors. If the type is opaque this list will be empty as
    /// the type doesn't have any public constructor.
    ///
    /// ```gleam
    /// type Result(a, b) {
    ///   Ok(a)
    ///   Error(b)
    /// }
    /// // `Ok` and `Error` are the type constructors
    /// // of the `Error` type.
    /// ```
    ///
    constructors: List(TypeConstructor),
  )
}

/// A Gleam type constructor.
///
/// ```gleam
/// type Result(a, b) {
///   Ok(a)
///   Error(b)
/// }
/// // `Ok` and `Error` are the type constructors
/// // of the `Error` type.
/// ```
///
pub type TypeConstructor {
  TypeConstructor(
    /// The type constructor's documentation comment (that is every line
    /// preceded by `///`).
    ///
    documentation: Option(String),
    name: String,
    /// The parameters required by the constructor.
    ///
    /// ```gleam
    /// type Box(a) {
    ///   Box(content: a)
    /// //    ^^^^^^^^^^ The `Box` constructor has a single
    /// //               labelled argument.
    /// }
    /// ```
    ///
    parameters: List(Parameter),
  )
}

/// A parameter (that might be labelled) of a module function or type
/// constructor.
///
/// ```gleam
/// pub fn map(over list: List(a), with fun: fn(a) -> b) -> b { todo }
/// //         ^^^^^^^^^^^^^^^^^^ A labelled parameter.
/// ```
///
pub type Parameter {
  Parameter(label: Option(String), type_: Type)
}

/// A Gleam constant.
///
/// ```gleam
/// pub const my_favourite_number = 11
/// ```
///
pub type Constant {
  Constant(
    /// The constant's documentation comment (that is every line preceded by
    /// `///`).
    ///
    documentation: Option(String),
    /// If the constant is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    implementations: Implementations,
    type_: Type,
  )
}

/// A Gleam function definition.
///
/// ```gleam
/// pub fn reverse(list: List(a)) -> List(a) { todo }
/// ```
pub type Function {
  Function(
    /// The function's documentation comment (that is every line preceded by
    /// `///`).
    ///
    documentation: Option(String),
    /// If the function is deprecated this will hold the reason of the
    /// deprecation.
    ///
    deprecation: Option(Deprecation),
    implementations: Implementations,
    parameters: List(Parameter),
    return: Type,
  )
}

/// A deprecation notice that can be added to definition using the
/// `@deprecated` annotation.
///
pub type Deprecation {
  Deprecation(message: String)
}

/// Metadata about how a value is implemented and the targets it supports.
///
pub type Implementations {
  Implementations(
    /// Set to `True` if the const/function has a pure Gleam implementation
    /// (that is, it never uses external code).
    /// Being pure Gleam means that the function will support all Gleam
    /// targets, even future ones that are not present to this day.
    ///
    /// Consider the following function:
    ///
    /// ```gleam
    /// @external(erlang, "foo", "bar")
    /// pub fn a_random_number() -> Int {
    ///   4
    ///   // This is a default implementation.
    /// }
    /// ```
    ///
    /// The implementations for this function will look like this:
    ///
    /// ```gleam
    /// Implementations(
    ///   gleam: True,
    ///   uses_erlang_externals: True,
    ///   uses_javascript_externals: False,
    /// )
    /// ```
    ///
    /// - `gleam: True` means that the function has a pure Gleam implementation
    ///   and thus it can be used on all Gleam targets with no problems.
    /// - `uses_erlang_externals: True` means that the function will use Erlang
    ///   external code when compiled to the Erlang target.
    /// - `uses_javascript_externals: False` means that the function won't use
    ///   JavaScript external code when compiled to JavaScript. The function can
    ///   still be used on the JavaScript target since it has a pure Gleam
    ///   implementation.
    ///
    gleam: Bool,
    /// Set to `True` if the const/function is defined using Erlang external
    /// code. That means that the function will use Erlang code through FFI when
    /// compiled for the Erlang target.
    ///
    uses_erlang_externals: Bool,
    /// Set to `True` if the const/function is defined using JavaScript external
    /// code. That means that the function will use JavaScript code through FFI
    /// when compiled for the JavaScript target.
    ///
    /// Let's have a look at an example:
    ///
    /// ```gleam
    /// @external(javascript, "foo", "bar")
    /// pub fn javascript_only() -> Int
    /// ```
    ///
    /// It's implementations field will look like this:
    ///
    /// ```gleam
    /// Implementations(
    ///   gleam: False,
    ///   uses_erlang_externals: False,
    ///   uses_javascript_externals: True,
    /// )
    /// ```
    ///
    /// - `gleam: False` means that the function doesn't have a pure Gleam
    ///   implementations. This means that the function is only defined using
    ///   externals and can only be used on some targets.
    /// - `uses_erlang_externals: False` the function is not using external
    ///   Erlang code.
    /// - `uses_javascript_externals: True` the function is using JavaScript
    ///   external code.
    ///
    uses_javascript_externals: Bool,
    /// Whether this function can be used when targetting Erlang.
    ///
    /// This is false when it or a function it calls only has a JavaScript
    /// implementation.
    can_run_on_erlang: Bool,
    /// Whether this function can be used when targetting JavaScript.
    ///
    /// This is false when it or a function it calls only has a Erlang
    /// implementation.
    can_run_on_javascript: Bool,
  )
}

/// A Gleam type.
///
pub type Type {
  /// A tuple type like `#(Int, Float)`.
  ///
  Tuple(elements: List(Type))
  /// A function type like `fn(Int, a) -> List(a)`.
  ///
  Fn(parameters: List(Type), return: Type)
  /// A type variable.
  ///
  /// ```gleam
  /// pub fn foo(value: a) -> a { todo }
  /// //                ^ This is a type variable.
  /// ```
  ///
  Variable(id: Int)
  /// A custom named type.
  /// ```gleam
  /// let value: Bool = True
  /// //         ^^^^ Bool is a named type coming from Gleam's prelude
  /// ```
  ///
  /// All prelude types - like Bool, String, etc. - are named types as well.
  /// In that case, their package is an empty string `""` and their module
  /// name is the string `"gleam"`.
  ///
  Named(
    name: String,
    /// The package the type comes from.
    ///
    package: String,
    /// The module the type is defined in.
    ///
    module: String,
    /// The concrete type's type parameters
    /// .
    /// ```gleam
    /// let result: Result(Int, e) = Ok(1)
    /// //                ^^^^^^^^ The `Result` named type has 2 parameters.
    /// //                         In this case it's the `Int` type and a
    /// //                         type variable.
    /// ```
    ///
    parameters: List(Type),
  )
}

// --- DECODERS ----------------------------------------------------------------

pub fn decoder() -> Decoder(Package) {
  use name <- decode.field("name", decode.string)
  use version <- decode.field("version", decode.string)
  use gleam_version_constraint <- decode.field(
    "gleam-version-constraint",
    decode.optional(decode.string),
  )
  use modules <- decode.field(
    "modules",
    decode.dict(decode.string, module_decoder()),
  )
  Package(name:, version:, gleam_version_constraint:, modules:)
  |> decode.success
}

pub fn module_decoder() -> Decoder(Module) {
  use documentation <- decode.field("documentation", decode.list(decode.string))
  use type_aliases <- decode.field(
    "type-aliases",
    decode.dict(decode.string, type_alias_decoder()),
  )
  use types <- decode.field(
    "types",
    decode.dict(decode.string, type_definition_decoder()),
  )
  use constants <- decode.field(
    "constants",
    decode.dict(decode.string, constant_decoder()),
  )
  use functions <- decode.field(
    "functions",
    decode.dict(decode.string, function_decoder()),
  )
  Module(documentation:, type_aliases:, types:, constants:, functions:)
  |> decode.success
}

pub fn type_alias_decoder() -> Decoder(TypeAlias) {
  use documentation <- decode.field(
    "documentation",
    decode.optional(decode.string),
  )
  use deprecation <- decode.field(
    "deprecation",
    decode.optional(deprecation_decoder()),
  )
  use parameters <- decode.field("parameters", decode.int)
  use alias <- decode.field("alias", type_decoder())
  TypeAlias(documentation:, deprecation:, parameters:, alias:)
  |> decode.success
}

pub fn type_definition_decoder() -> Decoder(TypeDefinition) {
  use documentation <- decode.field(
    "documentation",
    decode.optional(decode.string),
  )
  use deprecation <- decode.field(
    "deprecation",
    decode.optional(deprecation_decoder()),
  )
  use parameters <- decode.field("parameters", decode.int)
  use constructors <- decode.field(
    "constructors",
    decode.list(constructor_decoder()),
  )
  TypeDefinition(documentation:, deprecation:, parameters:, constructors:)
  |> decode.success
}

pub fn constant_decoder() -> Decoder(Constant) {
  use documentation <- decode.field(
    "documentation",
    decode.optional(decode.string),
  )
  use deprecation <- decode.field(
    "deprecation",
    decode.optional(deprecation_decoder()),
  )
  use implementations <- decode.field(
    "implementations",
    implementations_decoder(),
  )
  use type_ <- decode.field("type", type_decoder())
  Constant(documentation:, deprecation:, implementations:, type_:)
  |> decode.success
}

pub fn function_decoder() -> Decoder(Function) {
  use documentation <- decode.field(
    "documentation",
    decode.optional(decode.string),
  )
  use deprecation <- decode.field(
    "deprecation",
    decode.optional(deprecation_decoder()),
  )
  use implementations <- decode.field(
    "implementations",
    implementations_decoder(),
  )
  use parameters <- decode.field("parameters", decode.list(parameter_decoder()))
  use return <- decode.field("return", type_decoder())
  Function(documentation:, deprecation:, implementations:, parameters:, return:)
  |> decode.success
}

pub fn deprecation_decoder() -> Decoder(Deprecation) {
  use message <- decode.field("message", decode.string)
  decode.success(Deprecation(message:))
}

pub fn constructor_decoder() -> Decoder(TypeConstructor) {
  use documentation <- decode.field(
    "documentation",
    decode.optional(decode.string),
  )
  use name <- decode.field("name", decode.string)
  use parameters <- decode.field("parameters", decode.list(parameter_decoder()))
  decode.success(TypeConstructor(documentation:, name:, parameters:))
}

pub fn implementations_decoder() -> Decoder(Implementations) {
  use gleam <- decode.field("gleam", decode.bool)
  use uses_erlang_externals <- decode.field(
    "uses-erlang-externals",
    decode.bool,
  )
  use uses_javascript_externals <- decode.field(
    "uses-javascript-externals",
    decode.bool,
  )
  use can_run_on_erlang <- decode.optional_field(
    "can-run-on-erlang",
    option.None,
    decode.optional(decode.bool),
  )
  use can_run_on_javascript <- decode.optional_field(
    "can-run-on-javascript",
    option.None,
    decode.optional(decode.bool),
  )
  let can_run_on_erlang = can_run_on_erlang |> option.unwrap(gleam)
  let can_run_on_javascript = can_run_on_javascript |> option.unwrap(gleam)
  decode.success(Implementations(
    gleam:,
    uses_erlang_externals:,
    uses_javascript_externals:,
    can_run_on_erlang:,
    can_run_on_javascript:,
  ))
}

pub fn parameter_decoder() -> Decoder(Parameter) {
  use label <- decode.field("label", decode.optional(decode.string))
  use type_ <- decode.field("type", type_decoder())
  decode.success(Parameter(label:, type_:))
}

pub fn type_decoder() -> Decoder(Type) {
  use kind <- decode.field("kind", decode.string)
  case kind {
    "variable" -> {
      use id <- decode.field("id", decode.int)
      decode.success(Variable(id:))
    }

    "tuple" -> {
      use elements <- decode.field("elements", decode.list(type_decoder()))
      decode.success(Tuple(elements:))
    }

    "named" -> {
      use name <- decode.field("name", decode.string)
      use package <- decode.field("package", decode.string)
      use module <- decode.field("module", decode.string)
      use parameters <- decode.field("parameters", decode.list(type_decoder()))
      decode.success(Named(name:, package:, module:, parameters:))
    }

    "fn" -> {
      use parameters <- decode.field("parameters", decode.list(type_decoder()))
      use return <- decode.field("return", type_decoder())
      decode.success(Fn(parameters:, return:))
    }

    _unknown_tag ->
      decode.failure(Variable(id: 0), "String of variable, tuple, named, or fn")
  }
}
