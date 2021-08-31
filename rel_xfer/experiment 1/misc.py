# misc functions
# thanks to Garrett Honke and Nolan Conaway for these functions

from psychopy import visual, event, core, gui
import os, random as rnd, socket, sys, shutil, datetime
import numpy as np


# Creates folder if it doesnt exist
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def check_directory(dir): 
    if not os.path.exists(dir):
        os.makedirs(dir)


# Create text entry window and return subject info 
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def get_subject_info(experiment_name, conditions, data_location):
    ss_info = []
    pc = socket.gethostname()
    my_Dlg = gui.Dlg(title=experiment_name)
    my_Dlg.addText('Subject Info')
    my_Dlg.addField('ID:', tip='or subject code')
    my_Dlg.addField('Condition:', rnd.choice(conditions), choices = conditions)
    my_Dlg.show()
    if not my_Dlg.OK:
        print 'User Terminated'
        core.quit()
        
    subject_info = [str(i) for i in my_Dlg.data]
    
    if subject_info[0]=='':
        core.quit()
    else: 
        id = subject_info[0]
        condition = subject_info[1]
        subject_file = (data_location + pc + '-' + experiment_name + '-' + 
            condition + '-' + id + '.csv')
        while os.path.exists(subject_file) == True:
            subject_file = (data_location + pc + '-' + experiment_name + '-' + 
                condition + '-' + id + '.csv' + '_dupe')
        return [int(id),int(condition),subject_file]


# Re-assigns dimension and feature values based on counterbalance lists
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def counterbalance(subject_number, stimuli, feature_balance_list,
                    dimension_balance_list, feature_names):

    # Select a counterbalance condition based on subject
    n_conditions = len(feature_balance_list)
    condition    = subject_number % n_conditions
    
    # Balance features
    feature_assignment = feature_balance_list[condition] == 1
    print ['FLIPPED FEATURES:', feature_assignment]
    for i in stimuli:
        features = i[2]
        features[feature_assignment] = 1 - features[feature_assignment]
        stimuli[stimuli.index(i)][2] = features
    # Balance dimensions and create labels    
    dimension_assignment = dimension_balance_list[condition] - 1
    orig_feature_names   = list(feature_names)
    count = 0
    for i in dimension_assignment:
        feature_names[count] = orig_feature_names[i]
        count = count + 1
    print ['DIMENSION SHUFFLE:', dimension_assignment,feature_names]
    for i in stimuli:
        features = i[2]
        stimuli[stimuli.index(i)][2] = features[dimension_assignment]
       
    print ''
    return [stimuli, condition, feature_names, dimension_assignment]


# copies the data file to a series of dropbox folders
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def copy_2_db(file_name, experiment_name):
    copy_folders = [ #add your own!
        'C:\\Users\\klab\\Dropbox\\PSYCHOPY DATA\\' + experiment_name + '\\',
        'C:\\Users\\klab\\Dropbox\\sean\\PSYCHOPY DATA\\' + experiment_name + '\\']

    for i in copy_folders:
        check_directory(i)
        shutil.copy(file_name,i)
        

# Flatten a list
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def flatten(LIST):
    for i in LIST:
        if isinstance(i, (list, tuple)):
            for j in flatten(i):
                yield j
        else:
            yield i


# Converts an array into a list  
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def array_2_list(data,int_convert):
    result = np.array(data)
    result[np.isnan(result)] = -1 #convert nan to -1

    if int_convert: #convert to integer if desired
        result = result.astype(int)
        
    result = result.tolist()
    return result   


# takes in a string and return a list of integers; a=0, b=1, ... _=nan
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def str_2_prop(string):
    features = np.tile(np.nan, [1, len(string)])[0]
    for dimension in range(0,len(string)):
        character = string[dimension]
        if character != '_':
             features[dimension] = (ord(character) - ord('a'))
    return features


# Present instructions  
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def present_instructions(win, stim, text, phase):
    event.clearEvents()
    original_position = stim.pos
    stim.setPos       = [0.0,0.0]
    stim.alignVert    = 'center'
    
    # Search text for instructions matching phase
    for i in text:
        if i[0] == phase:
            instructs = i[1]
            break
            
    # Draw text and wait for key press
    stim.setText(instructs)
    stim.draw()
    win.flip()
    core.wait(2)
    if 'q' in event.waitKeys(keyList = ['q','space']):
        print 'User Terminated'
        core.quit()
    
    stim.alignVert = 'top'
    stim.setPos = original_position
    event.clearEvents() 


# draw a list of objects with any length/dimensions
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def drawall(win,objects):
    for i in objects:
        if isinstance(i, (list, tuple)):
            for j in flatten(i):
                j.draw()
        else:
            i.draw()
    win.flip()


# monitor buttons, when clicked return click result
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def button_gui(cursor, timer, buttons, labels):

    # Clear events
    timer.reset()
    cursor.clickReset()
    event.clearEvents() 
    
    # Iterate until response
    while True:
        
        # Quit if desired
        if 'q' in event.getKeys():
            print 'User Terminated'
            core.quit()
            
        # Check to see if any stimulus has been clicked on
        for i in buttons:
            if cursor.isPressedIn(i):
                return [labels[buttons.index(i)], timer.getTime()]


# Find images with a provided set of properties
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def find_stimulus(stims, comparison):
    comparison = np.array(comparison)
    comparison[np.isnan(comparison)] = -1
    for i in stims:
        features = np.array(i[2])
        features[np.isnan(features)] = -1
        if np.array_equal(features, comparison):
            return i


# Program waits for a mouse click to continue
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def click_to_continue(cursor):
    event.clearEvents()
    cursor.clickReset() 
    while cursor.getPressed()==[False,False,False]:
        cursor.getPressed()
        if event.getKeys(keyList = 'q'):
            print 'User Terminated'
            core.quit()


# Determines which category a stimulus is in 
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def which_category(features, valid_egs):

    # convert to list if necessary
    if 'list' not in str(type(features)):
        features = array_2_list(features,True)
    
    category = -1
    for i in valid_egs:
        cat_num = valid_egs.index(i)
        if features in i:
            category = cat_num
            
    return category


# Do the initial blankcreen-fixcross start to a trial
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def start_trial(win, isi, fix_cross):
    fix_cross.draw()
    win.flip()
    core.wait(isi)   


# writes list to file
def write_file(file_name, data, delim):
    data_file = open(file_name, 'w')
    for line in data: #iterate over items in data list
        current_line = '\n' #start each line with a newline
        for j in line: #add each item onto the current line

            if isinstance(j, (list, tuple)): #check if item is a list
                for k in j:
                    current_line = current_line + str(k) + delim
            else:
                current_line = current_line + str(j) + delim
                
##        write current line
        data_file.write(current_line)
    data_file.close()  


# Finds approprtiate inference images
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
def combine_features(original, addition):
    new_properties = np.array(original)
    current_feature = 0
    for i in new_properties:
        if np.isnan(i):
            new_properties[current_feature] = addition[current_feature]
        current_feature = current_feature + 1
    return new_properties

