var shuffle = function (array) {

	var currentIndex = array.length;
	var temporaryValue, randomIndex;

	// While there remain elements to shuffle...
	while (0 !== currentIndex) {
		// Pick a remaining element...
		randomIndex = Math.floor(Math.random() * currentIndex);
		currentIndex -= 1;

		// And swap it with the current element.
		temporaryValue = array[currentIndex];
		array[currentIndex] = array[randomIndex];
		array[randomIndex] = temporaryValue;
	}

	return array;

};

var comp_clauses = [
  "the benefits of getting vaccinated far outweigh the risks",
  "many diseases can be contained with vaccines",
  "vaccines are a safe and effective way to save lives",
  "vaccines are effective at preventing diseases like measles",
  "there is no link between vaccines and autism",
  "the link between vaccines and autism is unfounded",
  "vaccines do not cause autism"
]

shuffled_comp_clauses = shuffle(comp_clauses.slice())

// randomly choose comp_clauses for items

var sentences = [
  {subject: "Scientists", verb: "say", comp: shuffled_comp_clauses[0]},
  {subject: "Scientists", verb: "believe", comp: shuffled_comp_clauses[1]},
  {subject: "Scientists", verb: "think", comp: shuffled_comp_clauses[2]},
  {subject: "Scientists", verb: "claim", comp: shuffled_comp_clauses[3]},
  {subject: "Scientists", verb: "argue", comp: shuffled_comp_clauses[4]},
  {subject: "Scientists", verb: "insist", comp: shuffled_comp_clauses[5]},
  {subject: "Scientists", verb: "suspect", comp: shuffled_comp_clauses[6]},
  {subject: "Scientists", verb: "find", comp: shuffled_comp_clauses[0]},
  {subject: "Scientists", verb: "show", comp: shuffled_comp_clauses[1]},
  {subject: "Scientists", verb: "point out", comp: shuffled_comp_clauses[2]},
]
