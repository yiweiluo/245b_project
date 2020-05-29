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
  "vaccines do not cause autism",
	"without the vaccine, the epidemic would have been far more widespread",
	"vaccines have saved countless lives",
	"we have basically eradicated measles, thanks to vaccines",
	"children should be required to get vaccinated",
	"vaccinating children is a crucial measure for public health",
	"concerns over vaccine injuries are generally unfounded",
	"the vaccine-autism link is entirely a myth",
	"the study linking vaccines to autism has been debunked",
	"vaccines were responsible for some cases of autism in children",
	"thimerosol, an active ingredient in the measles vaccine, can cause seizures",
	"the mercury contained in vaccines is linked to autism risk",
	"vaccines are not as effective as people think",
	"vaccines pose non-trivial risks",
	"parents should have a choice in whether they vaccinate their children",
	"vaccine benefits are exaggerated and vaccine risks are downplayed by pharmaceutical companies",
	"many children have shown adverse reactions to vaccines",
	"getting vaccinated can pose serious risks",
	"vaccines are not as safe as many people imagine",
	"the campaign to get children vaccinated is driven in large part by drug companies",
	"people have sometimes still gotten sick, even after being vaccinated against the illness",
	"vaccines do not guarantee immunity",
	"the absolute safety of vaccines is a misconception",
	"we need to be more aware of the risks posed by vaccines"
]

shuffled_comp_clauses = shuffle(comp_clauses.slice())

var subjects = ['Scientists','Researchers','Doctors']

verbs = ['say','believe','think','claim','argue','insist','suspect','find','show','point out']
shuffled_verbs = shuffle(verbs.slice())

// randomly choose comp_clauses for items
var sentences = []
for (var i = 0; i < 10; i++) {
	for (var j = 0; j < 3; j++) {
		sentences.push({subject: subjects[Math.floor(Math.random() * 3)], verb: shuffled_verbs[i], comp: shuffled_comp_clauses[i*3+j]});
	}
}

// var sentences = [
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "say", comp: shuffled_comp_clauses[0]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "believe", comp: shuffled_comp_clauses[1]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "think", comp: shuffled_comp_clauses[2]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "claim", comp: shuffled_comp_clauses[3]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "argue", comp: shuffled_comp_clauses[4]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "insist", comp: shuffled_comp_clauses[5]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "suspect", comp: shuffled_comp_clauses[6]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "find", comp: shuffled_comp_clauses[7]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "show", comp: shuffled_comp_clauses[8]},
//   {subject: subjects[Math.floor(Math.random() * 3)], verb: "point out", comp: shuffled_comp_clauses[9]},
// ]
