# transfer of relational categories - Experiment 1 second program (transfer classification) init

from psychopy import visual, event, core, gui
from misc import *
from socket import gethostname
from time import strftime
import os, random as rnd, sys, itertools
import numpy as np

# # # # # # # # # # # # # # # # # # # 
experiment_name = 'trancl'
conditions      = range(1,5)

text_color = [-1,-1,-1]
text_font  = 'Arial'
text_size  = 26

image_start        = [0,0]
#image_sizes        = [[195, 231],[130,154]] # size of images in pix

## Stimuli Locations
if sys.platform == 'darwin' or sys.platform == 'linux2':
    home_directory = os.getcwd()
    image_directory = os.getcwd() + '/stim_pilot/'
    subject_files = os.getcwd() + '/subjects/'
    
else:
    home_directory = os.getcwd()
    rocks_besod_image_directory = os.getcwd() + '\\rocks\\besod\\'
    rocks_makif_image_directory = os.getcwd() + '\\rocks\\makif\\'
    rocks_tolar_image_directory = os.getcwd() + '\\rocks\\tolar\\'
    mobiles_doppa_image_directory = os.getcwd() + '\\mobiles\\doppa\\'
    mobiles_wuggy_image_directory = os.getcwd() + '\\mobiles\\wuggy\\'
    mobiles_zibble_image_directory = os.getcwd() + '\\mobiles\\zibble\\'
    subject_files = os.getcwd() + '\\subjects\\'


# Get Subject Info and Start Window
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 
[subject_number, condition, subject_file] = get_subject_info(
    experiment_name, conditions, subject_files)

# light blue = [0.57,0.86,0.90]

# Create Window and Set Logging Option
if gethostname() not in ['klab1','klab2','klab3']:
    win = visual.Window(fullscr = True, units = 'pix', color = [0.96,.87,.65], screen = 0) 
else:
    win = visual.Window(fullscr = True, units = 'pix',color = [0.96,.87,.65])
    check_directory(os.getcwd() + '\\logfiles\\')
    log_file = os.getcwd() + '\\logfiles\\' + str(subject_number)+ '-logfile.txt'
    while os.path.exists(log_file):
       log_file = log_file + '_dupe.txt'
    log_file = open(log_file,'w')
    sys.stdout = log_file
    sys.stderr = log_file

# Get Current Date and Time
current_time = strftime("%a, %d %b %Y %X")

# Start Mouse and Timer
cursor = event.Mouse(visible = True, newPos = None, win = win)
timer = core.Clock() #clock

# Load Stimuli
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 
# rocks - besod
besod_stim = []
os.chdir(rocks_besod_image_directory)
image_list = os.listdir(rocks_besod_image_directory)
for image in image_list:
  imgIndex = image_list.index(image)
  if image != '.DS_Store' and image != 'desktop.ini':
    stim_set = image.split('-')[0]
    category = image.split('-')[1]
    example = image.split('-')[2]
    img = visual.ImageStim(win, image = rocks_besod_image_directory + image, name = example,interpolate=True)
    besod_stim.append([img, example, stim_set, category])

# rocks - makif
makif_stim = []
os.chdir(rocks_makif_image_directory)
image_list = os.listdir(rocks_makif_image_directory)
for image in image_list:
  imgIndex = image_list.index(image)
  if image != '.DS_Store' and image != 'desktop.ini':
    stim_set = image.split('-')[0]
    category = image.split('-')[1]
    example = image.split('-')[2]
    img = visual.ImageStim(win, image = rocks_makif_image_directory + image, name = example,interpolate=True)
    makif_stim.append([img, example, stim_set, category])

# rocks - tolar
tolar_stim = []
os.chdir(rocks_tolar_image_directory)
image_list = os.listdir(rocks_tolar_image_directory)
for image in image_list:
  imgIndex = image_list.index(image)
  if image != '.DS_Store' and image != 'desktop.ini':
    stim_set = image.split('-')[0]
    category = image.split('-')[1]
    example = image.split('-')[2]
    img = visual.ImageStim(win, image = rocks_tolar_image_directory + image, name = example,interpolate=True)
    tolar_stim.append([img, example, stim_set, category])

# mobiles - doppa
doppa_stim = []
os.chdir(mobiles_doppa_image_directory)
image_list = os.listdir(mobiles_doppa_image_directory)
for image in image_list:
  imgIndex = image_list.index(image)
  if image != '.DS_Store' and image != 'desktop.ini':
    stim_set = image.split('-')[0]
    category = image.split('-')[1]
    example = image.split('-')[2]
    img = visual.ImageStim(win, image = mobiles_doppa_image_directory + image, name = example,interpolate=True)
    doppa_stim.append([img, example, stim_set, category])

# mobiles - wuggy
wuggy_stim = []
os.chdir(mobiles_wuggy_image_directory)
image_list = os.listdir(mobiles_wuggy_image_directory)
for image in image_list:
  imgIndex = image_list.index(image)
  if image != '.DS_Store' and image != 'desktop.ini':
    stim_set = image.split('-')[0]
    category = image.split('-')[1]
    example = image.split('-')[2]
    img = visual.ImageStim(win, image = mobiles_wuggy_image_directory + image, name = example,interpolate=True)
    wuggy_stim.append([img, example, stim_set, category])

# mobiles - zibble
zibble_stim = []
os.chdir(mobiles_zibble_image_directory)
image_list = os.listdir(mobiles_zibble_image_directory)
for image in image_list:
  imgIndex = image_list.index(image)
  if image != '.DS_Store' and image != 'desktop.ini':
    stim_set = image.split('-')[0]
    category = image.split('-')[1]
    example = image.split('-')[2]
    img = visual.ImageStim(win, image = mobiles_zibble_image_directory + image, name = example,interpolate=True)
    zibble_stim.append([img, example, stim_set, category])


rnd.shuffle(doppa_stim)
rnd.shuffle(wuggy_stim)
rnd.shuffle(zibble_stim)

if condition == 1 or condition == 2:
  # shuffle stimuli
  rnd.shuffle(besod_stim)
  rnd.shuffle(makif_stim)
  rnd.shuffle(tolar_stim)
  # Classification
  class_stim = []
  for stim in range(0,6):
    class_stim.append(besod_stim[stim])
    class_stim.append(makif_stim[stim])
    class_stim.append(tolar_stim[stim])
  rnd.shuffle(class_stim)

else:
  # shuffle stim
  rnd.shuffle(doppa_stim)
  rnd.shuffle(wuggy_stim)
  rnd.shuffle(zibble_stim)
  # Classification
  class_stim = []
  for stim in range(0,6):
    class_stim.append(doppa_stim[stim])
    class_stim.append(wuggy_stim[stim])
    class_stim.append(zibble_stim[stim])
  rnd.shuffle(class_stim)

os.chdir(home_directory)

print '--------STIMULUS INFO--------'
print 'classification stim'
for i in class_stim:
  print i

# Set conditional variables by condition
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  # 
print '\n------------CONDITION INFO:------------'
print 'subject_number: ' + str(subject_number)
print 'condition: ' + str(condition)


subject_data = [[current_time], [condition, subject_number]]

# Load Instructions
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
from instructs import *
instructions = visual.TextStim(win, text = '', wrapWidth = 2000, 
    color = text_color, font = text_font, height = text_size)
fix_cross = visual.TextStim(win, text = '+', pos = image_start, wrapWidth = 1000,
	color = text_color, font = text_font, height = text_size)

continue_string = '\n\
\n\
Click anywhere to continue.'

# Initiate Experiment Phases
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #

num_training_blocks = 5 #experiment
# num_training_blocks = 2 # testing
# standard classification training
execfile('trans_classification.py')

# Exit screen
#  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #  #
instructions.setText(instruction_text[-1])
instructions.draw()
win.flip()
event.waitKeys()

print '\nExperiment completed'
if gethostname() in ['klab1', 'klab2', 'klab3']:
    copy_2_db(subject_file, experiment_name)
    log_file.close()
    os.system("TASKKILL /F /IM pythonw.exe")
