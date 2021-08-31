# Task instructions

instruction_text = [
    # #classification - 
    ["classify_rocks",
    "**Learning Phase**\n\
    \n\
    \n\
           Today, you will see pictures of rock arrangements created by\n\
    the Ladua culture.  The Ladua have names for three different types\n\
    of rock arrangements that they build (Besods, Makifs, and Tolars).\n\
    Your overall goal is to figure out what makes a given rock arrangement\n\
    belong to one of the three categories: Besods, Makifs, or Tolars.  You\n\
    will be tested on your knowledge of each type later.\n\
    \n\
    On each learning trial, you will see a rock arrangement and try to\n\
    figure out the correct category for each arrangement.  You will be\n\
    given feedback at the end of each trial to help you learn.  At\n\
    first you will have to guess, but before long you should become\n\
    quite good at recognizing the different categories.\n\
    \n\
    Press the spacebar when you are ready to begin."],
    
    # # errorless classification
    # #classification - 
    ["classify_mobiles",
    "**Learning Phase**\n\
    \n\
    \n\
           Today, you will see pictures of mobiles created by the Ladua\n\
    toy company.  The Ladua produce three different lines of mobiles \n\
    that they sell (Doppa, Wuggy, and Zibble).  Your overall goal is to\n\
    to figure out what makes a given mobile belong to one of the three\n\
    categories: Doppa, Wuggy, or Zibble.  You will be tested on your \n\
    knowledge of each type later.\n\
    \n\
    On each learning trial, you will see a mobile and try to figure\n\
    out the correct category for each mobile.  You will be given\n\
    feedback at the end of each trial to help you learn.  At first\n\
    you will have to guess, but before long you should become quite\n\
    good at recognizing the different categories.\n\
    \n\
    Press the spacebar when you are ready to begin."],

    # #classification - 
    ["study_rocks",
    "**Memory Phase**\n\
    \n\
    \n\
           In this part of the experiment, you will see a series of\n\
    rock arrangements presented one at a time.  Your task is to study\n\
    each arrangement so that you can remember whether or not it was\n\
    included in this set.  When you are finished studying each\n\
    arrangement, you can proceeded to the next one by clicking a\n\
    button on the screen.  You will be tested on your ability to\n\
    recognize these images later.\n\
    \n\
    Press the spacebar when you are ready to begin."],

    ["study_mobiles",
    "**Memory Phase**\n\
    \n\
    \n\
           In this part of the experiment, you will see a series of\n\
    mobiles presented one at a time.  Your task is to study each\n\
    mobile so that you can remember whether or not it was included\n\
    in this set. When you are finished studying each each mobile,\n\
    you can proceeded to the next one by clicking a button on the\n\
    screen.  You will be tested on your ability to recognize these\n\
    images later.\n\
    \n\
    Press the spacebar when you are ready to begin."],

    # # memory test
    ["recognition_rocks",
    "**Test Phase**\n\
    \n\
    \n\
           In the final stage of this experiment, you will be\n\
    shown a series of rock arrangements presented one at a time.\n\
    For each arrangement, you will be asked to answer whether or\n\
    not the arrangement was one of the items that you just studied\n\
    in the memory phase.  Selecting 'Yes' means that you remember\n\
    seeing the arrangement in the memory phase, while selecting\n\
    'No' means that you do not remember seeing the arrangement in\n\
    the memory phase.\n\
    \n\
    Press the spacebar when you are ready to begin."],

    ["recognition_mobiles",
    "**Test Phase**\n\
    \n\
    \n\
           In the final stage of this experiment, you will be\n\
    shown a series of mobiles presented one at a time.  For each\n\
    mobile, you will be asked to answer whether or not the mobile\n\
    was one of the items that you just studied in the memory phase.\n\
    Selecting 'Yes' means that you remember seeing the mobile in\n\
    the memory phase, while selecting 'No' means that you do not\n\
    remember seeing the mobile in the memory phase.\n\
    \n\
    Press the spacebar when you are ready to begin."],

    ["transfer_rocks",
    "    In this experiment, you will be shown images that belong to one of three\n\
    categories: Besod, Makif, and Tolar.  Your overall goal is to learn how to \n\
    correctly identify which category each image belongs to.  At first you will not\n\
    know how to identify which category each item belongs to, but before long you\n\
    should become quite good.  Try your best to gain mastery of the Besod, Makif,\n\
    and Tolar categories!\n\
    \n\
    Press the spacebar when you are ready to begin."],
    
    ["transfer_mobiles",
    "    In this experiment, you will be shown images that belong to one of three\n\
    categories: Doppa, Wuggy, and Zibble.  Your overall goal is to learn how to \n\
    correctly identify which category each image belongs to.  At first you will not\n\
    know how to identify which category each item belongs to, but before long you\n\
    should become quite good.  Try your best to gain mastery of the Doppa, Wuggy,\n\
    and Zibble categories!\n\
    \n\
    Press the spacebar when you are ready to begin."],

    ["hint_rocks",
    "    In this experiment, you will be shown images that belong to one of three\n\
    categories: Besod, Makif, and Tolar.  Your overall goal is to learn how to \n\
    correctly identify which category each image belongs to.  At first you will not\n\
    know how to identify which category each item belongs to, but before long you\n\
    should become quite good.  Try your best to gain mastery of the Besod, Makif,\n\
    and Tolar categories!\n\
    \n\
    To get you started here's a helpful *hint*: thinking about the Doppa, Wuggy,\n\
    and Zibble mobiles from the previous experiment will be helpful when learning\n\
    about the Besod, Makif, and Tolar categories.\n\
    \n\
    Press the spacebar when you are ready to begin."],
    
    ["hint_mobiles",
    "    In this experiment, you will be shown images that belong to one of three\n\
    categories: Doppa, Wuggy, and Zibble.  Your overall goal is to learn how to \n\
    correctly identify which category each image belongs to.  At first you will not\n\
    know how to identify which category each item belongs to, but before long you\n\
    should become quite good.  Try your best to gain mastery of the Doppa, Wuggy,\n\
    and Zibble categories!\n\
    \n\
    To get you started here's a helpful *hint*: thinking about the Besod, Makif,\n\
    and Tolar rock arrangement from the previous experiment will be helpful when learning\n\
    about the Doppa, Wuggy, and Zibble categories.\n\
    \n\
    Press the spacebar when you are ready to begin."],



    #exit screen
    '    Thank you for participating in this experiment!\n\
    Please inform the experimenter that you are ready\n\
    to move on to the next study.']
