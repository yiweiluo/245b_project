# Linguist 245 resources for projects

## MTurk experiment template

Here we are using a template for web-based experiments developed and used in Noah Goodman's CoCoLab.

**Excursion**: Since the original repo (i.e., this repo) from which you forked your own project repo has been updated, we will first "pull" the changes from the upstream repo.
Below are the steps:

1. Open the terminal and cd into your local project repo (e.g., ling245exp under ling245all) and execute  
```
git remote -v
```
It should show a remote repo called `origin` that corresponds to your project repo on GitHub.
Now we add reference to the upstream repo  
```
git remote add upstream https://github.com/Ciyang/experiment_template.git
```
Execute `git remote -v` again to confirm that the change is in effect.

2. Make sure you have comitted all your changes in the repo (use `git status` to check; it is OK to have untracked files). Execute
```
git pull upstream master
```
If you have not changed any files originally in the repo, then git will automatically merge the branches for you and prompt you to type in a commit message (e.g., type "course material update," save and exit).
Otherwise, you will probably have to merge the branches manually, resolving any conflicts by yourself. (This is why you were told not to change any existing files! But now you learn how to merge branches by yourself.)

Now you should see (using `ls` or the GUI file finder in your OS) that there is a 'resources' folder, which contains 4 folders: `css`, `html`, `js`, and `shared`.
They contain all the relevant files that power the template experiment.
We will go through them shortly, explain what they do, and you will be playing with them.
But since you do not want to change them directly (no tampering with files from the upstream repo, unless you know what you are doing), let us first make some copies so that you can work on them. Create a folder called `practice` under `ling245exp` and copy the 4 folders under `resources` into it.

The `shared` folder contains common files needed for the template and you generally do not need to worry about or change them (but you are encouraged to take a look at some point just to get a sense of what is in there). Everything else specific to the particular experiment is stored in the other 3 folders. We will be focusing on the `html` and `js` folders.

### HTML

The `html` folder contains  `template_minimal.html`, an experiment with a minimalist set-up.
We will go through it and explains what each part does.

1. The first part of the html file is between the head tags `<head> </head>`. Generally, you only need to make a few changes to reflect things specific to the current experiment (the part is close to the closing tag `</head>`).  
**HW: change the title (i.e., edit the content in between the title tags `<title> </title>`). Open the html file in a browser and confirm the change.** (This is changing the title of the window/tab in the browser, not the web page.)

2. In between the body tags `<body> </body>` are
 (mostly) the slides used in the whole experiment.
 Each slide is specified within the div tags as follows `<div class="slide" id="[slideID]"></div>`.
You can see that there are currently slides, whose ids are `i0`, `consent`, `instruction`,
`example`, `priming`, `critical`, `subj_info`, `thanks`.
In general, you do not need to edit the `consent`,
 `subj_info` and `critical` slides,
 and depending on the particular experiment design, you may have various number of slides between `example` and `subj_info` corresponding to different blocks in your experiment.

The HTML file only specifies the skeleton of your experiment, which includes
- Things that do not need to be changed throughout the experiment.  
**HW: Change the `i0` and `instruction` slides
 so that they suit your project.**

- Names/IDs and default values of the things that will change as the experiment unfolds. For instance, in the `priming` slides there are two
 paragraphs whose ids are `primingCondition` and
 `primingSentence`. We know that we want the paragraphs to be there, but the exact content depends on the condition and may also
 change as the experiment progresses.
 So we just specify their ids and leave some default content `{{}}` so that it would be
 clear if something goes wrong.
 The rest of the work is done using JavaScript, which we now turn to.

### JavaScript

The JavaScript file `template_minimal.js`
 in the  `js` folder specifies the flow of the experiment.
First, scroll towards the end and you will see a
 function called `init`.
This specifies the initialization process when the entire experiment is loaded. Generally you only need to do two things here: (i) randomly assign between-subject conditions to `exp.condition` and (ii) specify the array of blocks in `exp.structure`.

Next, scroll back to the beginning to the `make_slides` function.
This is where you specify the control flow for
 each slide, using the format
 ```
 slides.[slideName] = slide({...slide specification...});
 ```

Technically, the slide specification (including the curly brackets) is an object that is passed
 as argument to the `slide` function,
but the details do not matter too much for now.
Generally we just need to add a bunch of  `key: value` specifications, separated by comma.
Below are some common ones:

1. `name: "[name of the slide]"`

2. `start: function(){...stuff that happens at the beginning of the block...}` For instance, at
 the beginning of the example slide,
 the error message is hidden: `$(".err").hide();`

3. `present: [an array of objects]` If a block contains multiple trials, then the trials will
 be run making use of the first element in the array to the last. See `present_handle` below.

4. `present_handle: function(stim){...render the exact content to be presented in the slide for the current trial...}` Here `stim` is an element in the array specified in `present` above.
 See how the function is defined in the `slides.priming` to modify `primingCondition` and `primingSentence`.  
**HW: See the comments on how to define the `present` array that can be different under different conditions**

5.  `button: function(){...stuff that happens when the button is clicked on...}` This is the place to verify responses, log data and go to the next step of the experiment.  
For instance, look at how this is defined
 in the critical slide (`slides.critical`).
The first line retrieves the checked radio button. If in fact no button is checked, then
 the error message is shown. Otherwise the response is logged (see `log_responses` below) and the next trial/block starts.  
**HW: Define an appropriate button function for the example slide.** (One possibility: show the error message if the participant makes the "wrong" choice)

6. `log_responses: function(){...logging responses...}` This is usually done by calling the push function of an array.
To find out which array is relevant, take a look at `slides.thanks`.  
**Very important: absolutely make sure that you have logged all the relevant information by checking the output at the end of the experiment in the debug mode AND go through the process in the Sandbox.** The last thing you want after having spent hundreds of dollars is realizing that your data are completely useless because you forgot to log a crucial manipulation.  
For instance, just finish the current experiment and take a look at the output file at the bottom of the last page. Do you notice anything off? Think about how it should be fixed (Hint:take a closer look at the code).

Now you should have a sense of the basic flow of the experiment, but it probably will take a lot of trials and errors for you to get familiar with it.  
**HW: take a look at `slides.one_slider`, `slides.multi_slider`, `slides.vertical_sliders`, `slides.subj_info` to see how various response types are logged.
Try to modify the critical slide in various ways so that the responses are in the form of (i) a slider, (ii) a drop-down panel, (iii) a textbox. Pay attention to what kind of verification and error messages are suitable in each cases.**
