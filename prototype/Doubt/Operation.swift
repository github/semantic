/// An operation of diffing over terms or collections of terms.
public enum Operation<Recur, Term, Diff> {
	/// Indicates that diffing should compare the enclosed `Term`s.
	///
	/// When run, the enclosed function will be applied to the resulting `Diff`.
	case Recursive(Term, Term, Diff -> Recur)

	/// Represents a diff to be performed on a collection of terms identified by keys.
	case ByKey([String:Term], [String:Term], [String:Diff] -> Recur)

	/// Represents a diff to be performed over an array of terms by index.
	case ByIndex([Term], [Term], [Diff] -> Recur)
}


// MARK: - Functor

extension Operation {
	public func map<Other>(transform: Recur -> Other) -> Operation<Other, Term, Diff> {
		switch self {
		case let .Recursive(a, b, f):
			return .Recursive(a, b, f >>> transform)
		case let .ByKey(a, b, f):
			return .ByKey(a, b, f >>> transform)
		case let .ByIndex(a, b, f):
			return .ByIndex(a, b, f >>> transform)
		}
	}
}


import Prelude
