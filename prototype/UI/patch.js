function Patch(patch) {
	if (patch.delete != null) {
		this.before = termFromJSON(patch.delete);
	}
	if (patch.insert != null) {
		this.after = termFromJSON(patch.insert);
	}
	if (patch.replace != null) {
		this.before = termFromJSON(patch.replace.before);
		this.after = termFromJSON(patch.replace.after);
	}
	return this;
}
