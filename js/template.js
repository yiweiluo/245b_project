function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
       name : "i0",
       start: function() {
        exp.startT = Date.now();
       }
    });

    slides.bot = slide({
  name: "bot",
  start: function () {
    $('.err_msg').hide();
    exp.speaker = _.shuffle(["James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph", "Thomas", "Charles"])[0];
    exp.listener = _.shuffle(["Mary", "Patricia", "Jennifer", "Linda", "Elizabeth", "Barbara", "Susan", "Jessica", "Sarah", "Margaret"])[0];
    exp.lives = 0;
    var story = exp.speaker + ' says to ' + exp.listener + ': "It\'s a beautiful day, isn\'t it?"'
    var question = 'Who does ' + exp.speaker + ' talk to?';
    document.getElementById("s").innerHTML = story;
    document.getElementById("q").innerHTML = question;
  },
  button: function () {
    var textInput = document.getElementById("text_box").value;
    var listenerLowerCase = exp.listener.toLowerCase();
    var textInputLowerCase = textInput.toLowerCase();

    exp.data_trials.push({
      "slide_number_in_experiment": "bot_check",
      "stim": exp.lives,
      "response": textInput
    });

    if ((exp.lives < 3) && (textInputLowerCase === listenerLowerCase)) {
      exp.go();
    }
    else {
      $('.err_msg').hide();
      switch (exp.lives) {
        case 0:
          $('#err1').show();
          break;
        case 1:
          $('#err2').show();
          break;
        case 2:
          $('#disq').show();
          $('.button').hide();
          break;
        default:
          break;
      }
      exp.lives++;
    }
  },
  });

  slides.instructions_eval = slide({
    name : "instructions_eval",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.transition_eval = slide({
    name : "transition_eval",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.transition_prior = slide({
    name : "transition_prior",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  // slides.comments = slide({
  //   name : "comments",
  //   button : function() {
  //     this.log_responses();
  //     exp.go(); //use exp.go() if and only if there is no "present" data.
  //   }
  //
  //   log_responses : function() {
  //     exp.comments = {
  //       "comments" : $("#comments").val().trim()
  //     };
  // });

  slides.single_trial = slide({
    name: "single_trial",
    present: exp.all_stims,
    present_handle: function(stim) {
      $(".err").hide();
      this.stim = stim;
      console.log(stim.item);
      $(".display_condition").html("Do you like " + stim.item + "?");
    },
    button : function() {
      this.response = $('input[name="pet"]:checked').val();
      if (this.response == undefined) {
        $(".err").show();
      } else {
        this.log_responses();
        _stream.apply(this);
        // exp.data_trials.push({
        //   "trial_type" : "single_trial",
        //   "response" : response
        // });
        // exp.go(); //make sure this is at the *end*, after you log your data
      }
    },
    log_responses : function() {
      exp.data_trials.push({
        "trial_type" : "pet_trial",
        "pet" : this.stim.item,
        "response" : this.response
      });
    }
  });

  slides.one_slider = slide({
    name : "one_slider",

    /* trial information for this block
     (the variable 'stim' will change between each of these values,
      and for each of these, present_handle will be run.) */
    present : exp.all_stims,

    // need to shuffle and add cross-trials

    //this gets run only at the beginning of the block
    present_handle : function(stim) {
      $(".err").hide();
      $(".slider_table").hide();
      $(".help_2").hide();
      $(".prompt_b").hide();
      $(".button_2").hide();
      $(".button_1").hide();

      $('.button_1').prop('disabled', true);
        setTimeout(function(){
          $('.button_1').prop('disabled', false);
          // change button color
          $('.button_1').html("Done reading").show()
        }, 2000);

      this.stim = stim; //I like to store this information in the slide so I can record it later.

      $(".prompt_a").html(('"' + stim.subject + ' ' + stim.verb + " that " + stim.comp + "." + '"').italics())
      // $(".button_1").html("Done reading")
      $(".prompt_b").html((stim.comp.charAt(0).toUpperCase() + stim.comp.slice(1) + ".").italics())
      $(".help_1").show();
      $(".prompt_a").show();
      //$(".prompt_b").html((stim.comp + ".").italics())
      this.init_sliders();
      exp.sliderPost = null; //erase current slider value
    },

    button_1 : function() {
      $(".help_2").show();
      $(".prompt_b").show();
      $(".slider_table").show();
      $(".button_2").show()
      $(".button_1").hide()
    },

    button_2 : function() {
      if (exp.sliderPost == null) {
        $(".err").show();
      } else {
        this.log_responses();

        /* use _stream.apply(this); if and only if there is
        "present" data. (and only *after* responses are logged) */
        _stream.apply(this);
      }
    },

    init_sliders : function() {
      utils.make_slider("#single_slider", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    log_responses : function() {
      exp.data_trials.push({
        "trial_type" : "one_slider",
        "response" : exp.sliderPost,
        "stim" : this.stim
      });
    }
  });

  slides.multi_slider = slide({
    name : "multi_slider",
    present : [
      {"item":"Vaccines have high preventive health benefits and low risks of side effects"},
      {"item":"Healthy children should be required to be vaccinated to attend school because of potential health risk to others"}, // 0 to 1
      {"item":"Parents should be able to decide not to vaccinate their children, even if that may create health risks for others"},
      {"item":"Medical scientists understand the health risks and benefits of vaccines"}, // 1 to 0
    ],
    present_handle : function(stim) {
      $(".err").hide();
      this.stim = stim; //FRED: allows you to access stim in helpers

      this.sentence_types = _.shuffle(["generic"]);//, "negation", "always", "sometimes", "usually"]);
      var sentences = {
        "generic": stim.item + ".",
        // "negation": stim.item + ".",
        // "always": stim.item + ".",
        // "sometimes": stim.item + ".",
        // "usually": stim.item + "."
      };

      this.n_sliders = this.sentence_types.length;
      $(".slider_row").remove();
      for (var i=0; i<this.n_sliders; i++) {
        var sentence_type = this.sentence_types[i];
        var sentence = sentences[sentence_type];
        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="sentence' + i + '">' + sentence + '</td><td colspan="2"><div id="slider' + i + '" class="slider">-------[ ]--------</div></td></tr>');
        utils.match_row_height("#multi_slider_table", ".slider_target");
      }

      this.init_sliders(this.sentence_types);
      exp.sliderPost = [];
    },

    button : function() {
      if (exp.sliderPost.length < this.n_sliders) {
        $(".err").show();
      } else {
        this.log_responses();
        _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.
      }
    },

    init_sliders : function(sentence_types) {
      for (var i=0; i<sentence_types.length; i++) {
        var sentence_type = sentence_types[i];
        utils.make_slider("#slider" + i, this.make_slider_callback(i));
      }
    },
    make_slider_callback : function(i) {
      return function(event, ui) {
        exp.sliderPost[i] = ui.value;
      };
    },
    log_responses : function() {
      for (var i=0; i<this.sentence_types.length; i++) {
        var sentence_type = this.sentence_types[i];
        exp.data_trials.push({
          "trial_type" : "own_stance",
          "response" : exp.sliderPost[i]
        });
      }
    },
  });

  slides.vertical_sliders = slide({
    name : "vertical_sliders",
    present : _.shuffle([
      {
        "bins" : [
          {
            "min" : 0,
            "max" : 10
          },
          {
            "min" : 10,
            "max" : 20
          },
          {
            "min" : 20,
            "max" : 30
          },
          {
            "min" : 30,
            "max" : 40
          },
          {
            "min" : 40,
            "max" : 50
          },
          {
            "min" : 50,
            "max" : 60
          }
        ],
        "question": "How tall is tall?"
      }
    ]),
    present_handle : function(stim) {
      $(".err").hide();
      this.stim = stim;

      $("#vertical_question").html(stim.question);

      $("#sliders").empty();
      $("#bin_labels").empty();

      $("#sliders").append('<td> \
            <div id="slider_endpoint_labels"> \
              <div class="top">likely</div> \
              <div class="bottom">unlikely</div>\
            </div>\
          </td>')
      $("#bin_labels").append('<td></td>')

      this.n_sliders = stim.bins.length;
      for (var i=0; i<stim.bins.length; i++) {
        $("#sliders").append("<td><div id='vslider" + i + "' class='vertical_slider'>|</div></td>");
        $("#bin_labels").append("<td class='bin_label'>" + stim.bins[i].min + " - " + stim.bins[i].max + "</td>");
      }

      this.init_sliders(stim);
      exp.sliderPost = [];
    },

    button : function() {
      if (exp.sliderPost.length < this.n_sliders) {
        $(".err").show();
      } else {
        this.log_responses();
        _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.
      }
    },

    init_sliders : function(stim) {
      for (var i=0; i<stim.bins.length; i++) {
        utils.make_slider("#vslider" + i, this.make_slider_callback(i), "vertical");
      }
    },
    make_slider_callback : function(i) {
      return function(event, ui) {
        exp.sliderPost[i] = ui.value;
      };
    },
    log_responses : function() {
      for (var i=0; i<this.stim.bins.length; i++) {
        exp.data_trials.push({
          "trial_type" : "vertical_slider",
          "question" : this.stim.question,
          "response" : exp.sliderPost[i],
          "min" : this.stim.bins[i].min,
          "max" : this.stim.bins[i].max
        });
      }
    },
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        assess : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
        problems: $("#problems").val(),
        fairprice: $("#fairprice").val(),
        start_time: exp.startT,
        time_taken: (Date.now() - exp.startT)/60000,
        phaseseed: exp.phase_seed,
        comments: exp.comments,
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
          "comments": exp.comments,
          "time_in_minutes" : (Date.now() - exp.startT)/60000,
          "start_time": Date.now(),
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];
  exp.all_stims = _.shuffle(sentences); //can randomize between subject conditions here
  //exp.comp_clauses = _.shuffle(comp_clauses)
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  var structures = [["i0", "instructions_eval", "one_slider", 'transition_prior', 'multi_slider', 'subj_info', 'thanks'],["i0", 'multi_slider',"transition_eval",  "one_slider", 'subj_info', 'thanks']];
  var phase_seed = Math.floor(Math.random() * 2)
  exp.phase_seed = phase_seed
  exp.structure = structures[phase_seed];

  exp.data_trials = [];
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
