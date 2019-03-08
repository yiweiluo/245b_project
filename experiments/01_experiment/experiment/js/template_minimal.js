function make_slides(f) {
  var   slides = {};

/* For Ling245, no need to change the code
 for i0 and consent slides*/
  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.consent = slide({
     name : "consent",
     start: function() {
      exp.startT = Date.now();
      $("#consent_2").hide();
      exp.consent_position = 0;
     },
    button : function() {
      if(exp.consent_position == 0) {
         exp.consent_position++;
         $("#consent_1").hide();
         $("#consent_2").show();
      } else {
        exp.go(); //use exp.go() if and only if there is no "present" data.
      }
    }
  });

/*Consult the code in the consent slide if you
  want to break down very long instructions */
  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.example = slide({
    name: "example",
    start: function() {
      $(".err").hide();
      $('input[name=exChoice]:checked').prop('checked', false);
    },
    button : function() {
    // make sure participants understand
    // the task before they continue
      // response = $("#text_response").val();
      // if (response.length == 0) {
      //   $(".err").show();
      // } else {
      //   exp.data_trials.push({
      //     "trial_type" : "example",
      //     "response" : response
      //   });
      //   exp.go(); //make sure this is at the *end*, after you log your data
      // }
      exp.go(); //use exp.go() if and only if there is no "present" data.
    },
  });

  slides.priming = slide({
    name: "priming",
    start: function() {
      $("#primingCondition").html("This is a priming sentece with " + exp.condition);
    },


    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.)
      Remember to comment out the other
      specification of present below*/
    present: _.shuffle([
      {speaker: "John"},
      {speaker: "Mary"}
    ]),

    /* It might be the case that the
    array of things you want to present depends
    on the condition. A solution is
    to define the array when the condition
    is determined, e.g., in init().
    Remember to comment out the other
    specification of present above*/
    //present: _.shuffle(exp.primingStims),

    present_handle: function(stim){

      $("#primingCondition").html("This is a sentence with " + exp.condition);

      // use this version if present is directly given
      $("#primingSentence").html(stim.speaker +
       " said a sentence with " + exp.condition);

      // use this version if present depends on the condition
      //$("#primingSentence").html(stim);
      this.stim = stim; //you can store this information in the slide so you can record it later.
    },

    button : function() {
      _stream.apply(this);
    },
  });

  // slides.example = slide({
  //   name: "example",
  //   start: function() {
  //     $(".err").hide();
  //     $(".display_condition").html("You are in " + exp.condition + ".");
  //   },
  //   button : function() {
  //     response = $("#text_response").val();
  //     if (response.length == 0) {
  //       $(".err").show();
  //     } else {
  //       exp.data_trials.push({
  //         "trial_type" : "example",
  //         "response" : response
  //       });
  //       exp.go(); //make sure this is at the *end*, after you log your data
  //     }
  //   },
  // });

  slides.critical = slide({
    name : "critical",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : _.shuffle([
       "John and Mary laugh.",
       "Does John and Mary laugh?",
       "John and I am happy."
    ]),

    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();

      // uncheck the button and erase the previous value
      exp.criticalResponse == null;
      $('input[name=criticalChoice]:checked').prop('checked', false);
      $("#criticalSentence").html(stim);

      this.stim = stim; //you can store this information in the slide so you can record it later.

    },

    button : function() {
      //find out the checked option
      exp.criticalResponse = $('input[name=criticalChoice]:checked').val();

      // verify the response
      if (exp.criticalResponse == null) {
        $(".err").show();
      } else {
        this.log_responses();

        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },

    log_responses : function() {
      exp.data_trials.push({
        "trial_type" : "critical",
        //"sentence": this.stim, // don't forget to log the stimulus
        "response" : exp.criticalResponse
      });
    }
  });

  // slides.one_slider = slide({
  //   name : "one_slider",
  //
  //   /* trial information for this block
  //    (the variable 'stim' will change between each of these values,
  //     and for each of these, present_handle will be run.) */
  //   present : [
  //     {subject: "dog", object: "ball"},
  //     {subject: "cat", object: "windowsill"},
  //     {subject: "bird", object: "shiny object"},
  //   ],
  //
  //   //this gets run only at the beginning of the block
  //   present_handle : function(stim) {
  //     $(".err").hide();
  //
  //     this.stim = stim; //I like to store this information in the slide so I can record it later.
  //
  //
  //     $(".prompt").html(stim.subject + "s like " + stim.object + "s.");
  //     this.init_sliders();
  //     exp.sliderPost = null; //erase current slider value
  //   },
  //
  //   button : function() {
  //     if (exp.sliderPost == null) {
  //       $(".err").show();
  //     } else {
  //       this.log_responses();
  //
  //       /* use _stream.apply(this); if and only if there is
  //       "present" data. (and only *after* responses are logged) */
  //       _stream.apply(this);
  //     }
  //   },
  //
  //   init_sliders : function() {
  //     utils.make_slider("#single_slider", function(event, ui) {
  //       exp.sliderPost = ui.value;
  //     });
  //   },
  //
  //   log_responses : function() {
  //     exp.data_trials.push({
  //       "trial_type" : "one_slider",
  //       "response" : exp.sliderPost
  //     });
  //   }
  // });

  // slides.multi_slider = slide({
  //   name : "multi_slider",
  //   present : _.shuffle([
  //     {"critter":"Wugs", "property":"fur"},
  //     {"critter":"Blicks", "property":"fur"}
  //   ]),
  //   present_handle : function(stim) {
  //     $(".err").hide();
  //     this.stim = stim; //FRED: allows you to access stim in helpers
  //
  //     this.sentence_types = _.shuffle(["generic", "negation", "always", "sometimes", "usually"]);
  //     var sentences = {
  //       "generic": stim.critter + " have " + stim.property + ".",
  //       "negation": stim.critter + " do not have " + stim.property + ".",
  //       "always": stim.critter + " always have " + stim.property + ".",
  //       "sometimes": stim.critter + " sometimes have " + stim.property + ".",
  //       "usually": stim.critter + " usually have " + stim.property + "."
  //     };
  //
  //     this.n_sliders = this.sentence_types.length;
  //     $(".slider_row").remove();
  //     for (var i=0; i<this.n_sliders; i++) {
  //       var sentence_type = this.sentence_types[i];
  //       var sentence = sentences[sentence_type];
  //       $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="sentence' + i + '">' + sentence + '</td><td colspan="2"><div id="slider' + i + '" class="slider">-------[ ]--------</div></td></tr>');
  //       utils.match_row_height("#multi_slider_table", ".slider_target");
  //     }
  //
  //     this.init_sliders(this.sentence_types);
  //     exp.sliderPost = [];
  //   },
  //
  //   button : function() {
  //     if (exp.sliderPost.length < this.n_sliders) {
  //       $(".err").show();
  //     } else {
  //       this.log_responses();
  //       _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.
  //     }
  //   },
  //
  //   init_sliders : function(sentence_types) {
  //     for (var i=0; i<sentence_types.length; i++) {
  //       var sentence_type = sentence_types[i];
  //       utils.make_slider("#slider" + i, this.make_slider_callback(i));
  //     }
  //   },
  //   make_slider_callback : function(i) {
  //     return function(event, ui) {
  //       exp.sliderPost[i] = ui.value;
  //     };
  //   },
  //   log_responses : function() {
  //     for (var i=0; i<this.sentence_types.length; i++) {
  //       var sentence_type = this.sentence_types[i];
  //       exp.data_trials.push({
  //         "trial_type" : "multi_slider",
  //         "sentence_type" : sentence_type,
  //         "response" : exp.sliderPost[i]
  //       });
  //     }
  //   },
  // });
  //
  // slides.vertical_sliders = slide({
  //   name : "vertical_sliders",
  //   present : _.shuffle([
  //     {
  //       "bins" : [
  //         {
  //           "min" : 0,
  //           "max" : 10
  //         },
  //         {
  //           "min" : 10,
  //           "max" : 20
  //         },
  //         {
  //           "min" : 20,
  //           "max" : 30
  //         },
  //         {
  //           "min" : 30,
  //           "max" : 40
  //         },
  //         {
  //           "min" : 40,
  //           "max" : 50
  //         },
  //         {
  //           "min" : 50,
  //           "max" : 60
  //         }
  //       ],
  //       "question": "How tall is tall?"
  //     }
  //   ]),
  //   present_handle : function(stim) {
  //     $(".err").hide();
  //     this.stim = stim;
  //
  //     $("#vertical_question").html(stim.question);
  //
  //     $("#sliders").empty();
  //     $("#bin_labels").empty();
  //
  //     $("#sliders").append('<td> \
  //           <div id="slider_endpoint_labels"> \
  //             <div class="top">likely</div> \
  //             <div class="bottom">unlikely</div>\
  //           </div>\
  //         </td>')
  //     $("#bin_labels").append('<td></td>')
  //
  //     this.n_sliders = stim.bins.length;
  //     for (var i=0; i<stim.bins.length; i++) {
  //       $("#sliders").append("<td><div id='vslider" + i + "' class='vertical_slider'>|</div></td>");
  //       $("#bin_labels").append("<td class='bin_label'>" + stim.bins[i].min + " - " + stim.bins[i].max + "</td>");
  //     }
  //
  //     this.init_sliders(stim);
  //     exp.sliderPost = [];
  //   },
  //
  //   button : function() {
  //     if (exp.sliderPost.length < this.n_sliders) {
  //       $(".err").show();
  //     } else {
  //       this.log_responses();
  //       _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.
  //     }
  //   },
  //
  //   init_sliders : function(stim) {
  //     for (var i=0; i<stim.bins.length; i++) {
  //       utils.make_slider("#vslider" + i, this.make_slider_callback(i), "vertical");
  //     }
  //   },
  //   make_slider_callback : function(i) {
  //     return function(event, ui) {
  //       exp.sliderPost[i] = ui.value;
  //     };
  //   },
  //   log_responses : function() {
  //     for (var i=0; i<this.stim.bins.length; i++) {
  //       exp.data_trials.push({
  //         "trial_type" : "vertical_slider",
  //         "question" : this.stim.question,
  //         "response" : exp.sliderPost[i],
  //         "min" : this.stim.bins[i].min,
  //         "max" : this.stim.bins[i].max
  //       });
  //     }
  //   },
  // });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val()
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  //specify conditions
  exp.condition = _.sample(["comparatives", "multiple negations"]); //can randomize between subject conditions here
  //blocks of the experiment:
  exp.structure=["i0", "consent", "instructions", "example", "priming", "critical", 'subj_info', 'thanks'];

  exp.primingStims = {"comparatives": ["John ate more food than this burger.",
                              "Mary had more pets than Fido."],
             "multiple negations": ["No head injury is too severe to depair",
             "No head injury is too trivial to ignore"]
    }[exp.condition];

  // generally no need to change anything below
  exp.trials = [];
  exp.catch_trials = [];
  exp.data_trials = [];
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };

  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}
