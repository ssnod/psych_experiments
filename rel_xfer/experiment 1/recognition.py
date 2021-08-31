# memory recognition phase script

print '\n------RUNNING RECOGNITION------'
print '------------------------------------\n'

if condition == 1 or condition == 2 or condition == 3:
	phase = "recognition_rocks"
else:
	phase = "recognition_mobiles"

category_names = ['Yes', 'No']


print '-----TRAINING ITEMS:'
# Make classification buttons
buttons     = []
button_text = []

for i in category_names:
    button_num = category_names.index(i)
    
    buttons.append(visual.Rect(win, width = 150, height = 75))
    buttons[button_num].setFillColor([.8, .8, .8])
    buttons[button_num].setLineColor([-1, -1, -1])
    
    buttons[button_num].setPos([-100, -350])
    if button_num == 1:
        buttons[button_num].setPos([100, -350])
        
    # Create a text label
    button_text.append(visual.TextStim(win, text = category_names[button_num],
        height = text_size, font = text_font, color = text_color, 
        pos = buttons[button_num].pos))

# Present instructions and wait for response
present_instructions(win, instructions, instruction_text, phase)

# Iterate over blocks and trials
print '------executing trials------\n'

rnd.shuffle(recognition_test)
accuracy  = 0
adv = 0
trial_num = 1
for trial in recognition_test:
	while adv == 0:
		# Define trial properites
		print(trial)
		image = trial[0]
		file_name = trial[1]
		stim_set = trial[2]
		category = trial[3]
		item_class = trial[4]

		# Set task text
		task_text = 'Do you remember seeing this item during the memory phase?'
		structs = visual.TextStim(win, text = task_text, height = text_size,font = text_font,color = text_color, pos =[0, 350],wrapWidth = 2000)
		#instructions.setText(task_text)

		# Draw fix cross
		start_trial(win, .5, fix_cross)

		# Draw current stimuli
		#drawall(win, [image])
		#drawall(win, [image, instructions, buttons, button_text])
		drawall(win, [image, structs, buttons, button_text])
		core.wait(.5)

		# Run gui interface
		[response, rt] = button_gui(cursor, timer, buttons, category_names)
		drawall(win, [image])
		core.wait(.5)
		# Check correctness and return feedback
		if response == 'No' and item_class == 'New':
			accuracy = 1
			adv = 1
		elif response == 'Yes' and item_class == 'Old':
			accuracy = 1
			adv = 1
		else:
			accuracy = 0
			adv = 1
		
		# Click to continue
		# Log data
		current_trial = [subject_number, condition, phase, stim_set, trial_num,
			file_name, category, item_class, response, rt, accuracy]

		# Append to complete data and write file
		subject_data.append(current_trial)
		write_file(subject_file, subject_data, ',')

		# Print trial info
		print ('Trial ' + str(trial_num) + 
			'-' + ' information:')
		print ['presented image:', file_name]
		print ['actual:', item_class]
		print ['response:', response]
		print ['accuracy:', accuracy]


	#End Trial
	trial_num = trial_num + 1
	accuracy = 0
	adv = 0
