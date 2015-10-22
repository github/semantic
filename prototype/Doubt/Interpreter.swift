/// An interpreter of `Algorithm`s.
public struct Interpreter<Term: CofreeType> {
	/// The type of diffs constructed by `Interpreter`s.
	public typealias Diff = Free<Term.Leaf, Term.Annotation, Patch<Term>>

	/// Constructs an `Interpreter` parameterized by the `equal` and `comparable` tests on `Term`s, and the `cost` function for `Diff`s.
	///
	/// `equal` determines whether two terms are equal. This should typically be strict syntactic equality, not e.g. including any annotations of source ranges. If two terms are considered equal by this function, then an unchanged diff will be returned directly. No diffing will be performed, and `comparable` will not be applied to the terms at all.
	///
	/// `comparable` determines whether two terms should be considered comparable when encountered at some point within the tree. When diffing two `.Indexed` terms, this will cause diffing to delete one term and insert the other rather than recurring through the pair (possibly constructing a single replacement). This has far-reaching implications for performance, as it enables the caller to dramatically prune the search space.
	///
	/// `cost` computes the cost of a single diff. This is used when computing the diff of `.Indexed` terms: the algorithm selects the lowest-total-cost sequence of diffs which completely cover the arrays. This computation is performed a minimum of 2mn and a maximum of 3mn times, where m is the length of the first array and n is the length of the second. Therefore, it should be as efficient as possible, ideally no more than linear in the size of the diff.
	public init(equal: (Term, Term) -> Bool, comparable: (Term, Term) -> Bool, cost: Diff -> Int) {
		self.equal = equal
		self.comparable = comparable
		self.cost = cost
	}


	/// Computes a term comparable function from a categorizing function.
	public static func comparable<C>(categorize: Term -> Set<C>)(_ a: Term, _ b: Term) -> Bool {
		let c0 = categorize(a)
		let c1 = categorize(b)
		return c0 == c1 || !categorize(a).intersect(categorize(b)).isEmpty
	}

	/// Computes a diff cost function from a patch cost function.
	public static func cost(cost: Patch<Term> -> Int)(_ diff: Diff) -> Int {
		return diff.map(cost).reduce(0, combine: +)
	}


	/// Run the interpreter over a given pair of `Term`s, producing a `Diff` as its result.
	public func run(a: Term, _ b: Term) -> Diff {
		return recur(a, b) ?? .Replace(a, b)
	}


	private let equal: (Term, Term) -> Bool
	private let comparable: (Term, Term) -> Bool
	private let cost: Diff -> Int

	/// Diff `a` against `b`, if comparable.
	private func recur(a: Term, _ b: Term) -> Diff? {
		if equal(a, b) { return Diff.ana(Term.unwrap)(b) }
		guard comparable(a, b) else { return nil }

		let algorithm: Algorithm<Term, Diff>
		switch (a.unwrap, b.unwrap) {
		case let (.Keyed(a), .Keyed(b)):
			algorithm = .Roll(.ByKey(a, b, Syntax.Keyed >>> Diff.Roll >>> Algorithm.Pure))
		case let (.Indexed(a), .Indexed(b)):
			algorithm = .Roll(.ByIndex(a, b, Syntax.Indexed >>> Diff.Roll >>> Algorithm.Pure))
		default:
			algorithm = .Roll(.Recursive(a, b, Algorithm.Pure))
		}
		return recur(algorithm)
	}

	private func recur(algorithm: Algorithm<Term, Diff>) -> Diff? {
		switch algorithm {
		case let .Pure(diff):
			return diff

		case let .Roll(.Recursive(a, b, f)):
			// Recur structurally into both terms, patching differing sub-terms. This is akin to unification, except that it computes a patched tree instead of a substitution. It’s also a little like a structural zip on pairs of terms.
			switch (a.unwrap, b.unwrap) {
			case let (.Indexed(a), .Indexed(b)) where a.count == b.count:
				return recur(f(.Roll(.Indexed(zip(a, b).map(run)))))

			case let (.Keyed(a), .Keyed(b)) where Array(a.keys) == Array(b.keys):
				return recur(f(.Roll(.Keyed(Dictionary(elements: b.keys.map { ($0, self.run(a[$0]!, b[$0]!)) })))))

			default:
				// This must not call `recur` directly with `a` and `b`, as that would infinite loop if actually recursive.
				return recur(f(.Replace(a, b)))
			}

		case let .Roll(.ByKey(a, b, f)):
			// Perform [set reconciliation](https://en.wikipedia.org/wiki/Data_synchronization#Unordered_data) on the keys, followed by recurring into the values of the intersecting keys.
			let deleted = Set(a.keys).subtract(b.keys).map { ($0, Diff.Delete(a[$0]!)) }
			let inserted = Set(b.keys).subtract(a.keys).map { ($0, Diff.Insert(b[$0]!)) }
			let patched = Set(a.keys).intersect(b.keys).map { ($0, run(a[$0]!, b[$0]!)) }
			return recur(f(Dictionary(elements: deleted + inserted + patched)))

		case let .Roll(.ByIndex(a, b, f)):
			return recur(f(SES(a, b, cost: cost, recur: recur)))
		}
	}
}


// MARK: - Constrained constructors

extension Interpreter where Term.Leaf: Equatable {
	public init(comparable: (Term, Term) -> Bool, cost: Diff -> Int) {
		self.init(equal: Term.equals(==), comparable: comparable, cost: cost)
	}
}

extension Interpreter where Term: CofreeType, Term.Annotation: Categorizable {
	public init(equal: (Term, Term) -> Bool, cost: Diff -> Int) {
		self.init(equal: equal, comparable: Interpreter.comparable { $0.extract.categories }, cost: cost)
	}
}


import Prelude
