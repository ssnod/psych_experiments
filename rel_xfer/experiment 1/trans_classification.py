# transfer classification script

print '\n------RUNNING XFER CLASSIFICATION------'
print '------------------------------------\n'

if condition == 1 or condition == 2:
	category_names = ['Besod', 'Makif', 'Tolar']
	if condition == 1:
		phase = "transfer_rocks"
	else:
		phase = "hint_rocks"
else:
	category_names = ['Doppa', 'Wuggy', 'Zibble']
	if condition == 3:
		phase = "transfer_mobiles"
	else:
		phase = "hint_mobiles"

rnd.shuffle(category_names)

print '-----TRAINING ITEMS:'
# Make classification buttons
buttons     = []
button_text = []

for i in category_names:
    button_num = category_names.index(i)
    
    buttons.append(visual.Rect(win, width = 150, height = 75))
    buttons[button_num].setFillColor([0.57,0.86,0.90])
    #buttons[button_num].setFillColor([.8, .8, .8])
    buttons[button_num].setLineColor([-1, -1, -1])
    
    buttons[button_num].setPos([-200, -350])
    if button_num == 1:
        buttons[button_num].setPos([0, -350])
    elif button_num == 2:
    	buttons[button_num].setPos([200, -350])
        
    # Create a text label
    button_text.append(visual.TextStim(win, text = category_names[button_num],
        height = text_size, font = text_font, color = text_color, 
        pos = buttons[button_num].pos))

# Present instructions and wait for response
present_instructions(win, instructions, instruction_text, phase)

# Iterate over blocks and trials
print '------executing trials------\n'
for block_num in range(1, num_training_blocks + 1):
	rnd.shuffle(class_stim)
	training_block = class_stim
	accuracy  = 0
	adv = 0
	trial_num = 1
	for trial in training_block:
		while adv == 0:
			# Define trial properites
			print(trial)
			image = trial[0]
			file_name = trial[1]
			stim_set = trial[2]
			category = trial[3]

			# Set task text
			task_text = 'Click a button to select the correct category.'
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
			if response == category:
				feedback = ('Correct! This is a ' + category + '.')
				accuracy = 1
				adv = 1
			else:
				feedback = ('Incorrect... This is a ' + category + '.')
				accuracy = 0
				adv = 1
			
			structs.setText(feedback)
			drawall(win,[structs, image])
			core.wait(.5)

			# Click to continue
			structs.setText(feedback + continue_string)
			drawall(win, [image, structs])
			# Log data
			current_trial = [subject_number, condition, phase, stim_set, block_num, trial_num,
				file_name, category, response, rt,
				accuracy]

			# Append to complete data and write file
			subject_data.append(current_trial)
			write_file(subject_file, subject_data, ',')

			# Print trial info
			print ('Block ' + str(block_num) + ' Trial ' + str(trial_num) + 
				'-' + ' information:')
			print ['presented image:', file_name]
			print ['actual:', category]
			print ['response:', response]
			print ['accuracy:', accuracy]

			click_to_continue(cursor)

		#End Trial
		trial_num = trial_num + 1
		accuracy = 0
		adv = 0
		click_to_continue(cursor)
