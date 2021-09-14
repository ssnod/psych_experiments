var hEndorsement = {

    start: function(params, next) {
        params['results'].push(['experiment','phase','trial','block','stimId','category','endorseLabel','response','accuracy','rt'])

        /*   initialize   */
        space = oCanvas.create({
            canvas: hEndorsement.initCanvas(),
            background: 'rgba(245, 222, 166, 0.1)',
            // origin: { x: "center", y: "center" },
        })
        cx = space.canvasElement.width / 2
        cy = space.canvasElement.height / 2
        
        hEndorsement.initElements(params)
        hEndorsement.initDebugWindow(params)
        space.scenes.load('debug')
        // comment out to turn off debug window (delete or computer savvy subjects can uncover)

        x = new Date().getTime()
        experiment ='trancl'
        phase = 'hint_endorse'
        rt = 0
        response = null
        acc = null
        trial = 0
        block = 0
        block_order = hEndorsement.generateBlockOrder(params['objects'], params['labels']) // <-- initialize the first block order (you'll do this every block)

        correctCategory = block_order[trial][1]
        endorseLabel = block_order[trial][2]
        instructionsTxt.text = "Is this a ".concat(endorseLabel,"?\n*Hint*: think about the categories of rock arrangements")
        hEndorsement.initStim(block_order[trial][0])

        space.scenes.load('selection')


        /*   define event logic   */
        for (category of params['categories']) {
            responseBtns[category].btn.bind('click tap', function () {
                rt = new Date().getTime() - x

                response = this.category
                
                if(response == "Yes" && correctCategory == endorseLabel){
                    acc = true
                }else if(response == "No" && !(correctCategory == endorseLabel)){
                    acc = true
                }else{
                    acc = false
                }
                
                //acc = response == correctCategory

                if (acc == true) {
                    feedbackTxt.text = 'Correct! This is a '.concat(correctCategory)
                } else {
                    feedbackTxt.text = 'Incorrect... This is a '.concat(correctCategory)
                }

                space.scenes.unload('selection')
                space.scenes.load('feedback')

                setTimeout(function () {
                    space.scenes.load('continue')
                }, 200)
            })
        }


        continueBtn.bind('click tap', function () {
            space.scenes.unload('continue')
            space.scenes.unload('feedback')
            stimulus.remove()

            params['results'].push([experiment, phase, trial, block, block_order[trial][0].split('/').slice(-1)[0].split('.')[0], correctCategory, endorseLabel, response, acc, rt]) // pulled this variable from the main template
            trial += 1
            if (trial == block_order.length) {
                trial = 0
                block += 1
                if (block == params['n_blocks']) {
                    space.destroy(space)
                    document.getElementById('main').innerHTML = ''
                    next()
                } else {
                    block_order = hEndorsement.generateBlockOrder(params['objects'], params['labels'])
                }
            }
            
            setTimeout( function () {
                instructionsTxt.text = " "
                space.scenes.load('selection')
                hEndorsement.initStim(block_order[trial][0])
                correctCategory = block_order[trial][1]
                endorseLabel = block_order[trial][2]
                instructionsTxt.text = "Is this a ".concat(endorseLabel,"?\n*Hint*: think about the categories of rock arrangements")
                x = new Date().getTime()
            }, 200)

        })

        console.log('started spont_endorse')
    },


    initElements: function (params) {
        centerX = space.canvasElement.width / 2
        centerY = space.canvasElement.height / 2

        /*   Response Phase of Trial   */
        responseBtns = {}
        btnCompression = .6
        for (let [index, category] of params['categories'].entries()) {
            let xpos = ((space.canvasElement.width / params['categories'].length) * index)  +  ((space.canvasElement.width / params['categories'].length) / 2)
            responseBtns[category] = {
                'category': category,
                'btnLabel': params['btnLabels'][category],
            }

            responseBtns[category].btn = space.display.rectangle({
                x: xpos + (((space.canvasElement.width / 2) - xpos) * btnCompression),
                y: centerY + 225,
                origin: { x: "center", y: "center" },// align: 'center',
                width: 160,
                height: 60,
                fill: "rgba(145, 219, 229,1)",
                stroke: "4px #000",
                join: "round",
                category: category, // <-- CAREFUL! This is a custom added property to the oCanvas Base object; if they decide to ever include it, this will overwrite whatever function they decide the 'category' property should have (though low odds of that happening)
            })
            responseBtns[category].btn.addChild(
                space.display.text({
                    x: 0,
                    y: 0,
                    origin: { x: "center", y: "center" },// align: 'center',
                    font: "bold 25px serif",
                    text: responseBtns[category]['btnLabel'],
                    fill: "rgba(0,0,0,1)",
                    zIndex: "front",
                })
            )
        }

        instructionsTxt = space.display.text({
            x: centerX,
            y: centerY -220,
            origin: { x: "center", y: "center" }, align: 'center',
            align: "center",
            font: "32px serif",
            text: " ",
            fill: "rgba(0,0,0,1)",
            zIndex: "front",
        })

        space.scenes.create('selection', function () {
            for (category of params['categories']) {
                this.add(responseBtns[category].btn)
            }
            this.add(instructionsTxt)
        })


        /*   Feedback   */
        if (params['feedback'] == true) {
            feedbackTxt = space.display.text({
                x: cx,
                y: cy - 220,
                origin: { x: "center", y: "top" },
                font: "bold 30px serif",
                text: "Feedback Message",
                fill: "#000000"
            });
        } else {
            feedbackTxt = space.display.text({
                x: cx,
                y: cy,
                origin: { x: "center", y: "top" },
                font: "bold 30px serif",
                text: " ",
                fill: "#000000"
            });
        }
        space.scenes.create('feedback', function () {
            this.add(feedbackTxt)
        })



        /*   Continue Button   */
        continueBtn = space.display.rectangle({
            x: cx,
            y: cy + 250,
            origin: { x: "center", y: "center" }, align: 'center',
            width: 160,
            height: 60,
            fill: "rgba(145, 219, 229,1)",
            stroke: "4px #000",
            join: "round",
        })
        continueBtn.addChild(
            space.display.text({
                x: 0,
                y: 0,
                origin: { x: "center", y: "center" }, align: 'center',
                align: "center",
                font: "bold 25px serif",
                text: "Next Trial",
                fill: "rgba(0,0,0,1)",
                // zIndex: "front",
            })
        )
        space.scenes.create('continue', function () {
            this.add(continueBtn)
        })
    },


    initStim: function (image) {
        centerX = space.canvasElement.width / 2
        centerY = space.canvasElement.height / 2

        stimulus = space.display.image({
            x: centerX,
            y: centerY-25,
            origin: { x: "center", y: "center" }, align: 'center',
            image: image,
        })
        space.addChild(stimulus)
    },


    generateBlockOrder: function (objects, labels) {
        let block = []
        idx = hEndorsement.shuffle([...Array(labels.length).keys()]) // from ben @ https://stackoverflow.com/a/10050831
        
        console.log(idx)
        for (let i of idx) {
        
            if(labels[i] == "Doppa"){
                elabels = ["Doppa", "Doppa","Wuggy","Zibble"]
                elabels = hEndorsement.shuffle(elabels)
            }else if(labels[i] == "Wuggy"){
                elabels = ["Wuggy", "Wuggy","Doppa","Zibble"]
                elabels = hEndorsement.shuffle(elabels)
            }else{
                elabels = ["Zibble", "Zibble","Doppa","Wuggy"]
                elabels = hEndorsement.shuffle(elabels)
            }
        
            block.push([objects[i], labels[i], elabels[0]])
        }
        console.log(block)
        return block
    },


    shuffle: function (array) { // from mike bostok @ https://bost.ocks.org/mike/shuffle/
        var m = array.length, t, i;

        // While there remain elements to shuffle…
        while (m) {

            // Pick a remaining element…
            i = Math.floor(Math.random() * m--);

            // And swap it with the current element.
            t = array[m];
            array[m] = array[i];
            array[i] = t;
        }

        return array;
    
    },


    initCanvas: function () {
        canvas = document.createElement('canvas', {id: 'canvas', style: 'position:absolute;'})
        document.getElementById('main').appendChild(canvas)
        
        var ctx = canvas.getContext('2d');
        // ctx.lineWidth = 2;
        ctx.canvas.width  = window.innerWidth - 20;
        ctx.canvas.height = window.innerHeight - 20;

        window.addEventListener('resize', function () {
            space.width = window.innerWidth - 20;
            space.height = window.innerHeight - 20;
        }, false);

        return canvas
    },

    initDebugWindow: function (params) {
        lastBtnClick = ''

        debugWindow = space.display.rectangle({
            x: 10,
            y: 10,
            // origin: { x: "center", y: "center" }, align: 'center',
            width: 200,
            height:300,
            fill: "rgb(255,255,255)",
            stroke: "3px black",
            join: "round",
        })

        debugInfo = space.display.text({
            x: 30,
            y: 30,
            // origin: { x: "center", y: "center" },
            font: "bold 12px sans-serif",
            text: "debug",
            fill: "black",
            zIndex: "front",
        })

        debugWindow.addChild(debugInfo)

        close = space.display.rectangle({
            x: 12,
            y: 12,
            origin: { x: "center", y: "center" }, align: 'center',
            width: 20,
            height:20,
            fill: "rgb(190,190,190)",
            stroke: "2px black",
            join: "round",
        })
        close.addChild(
            space.display.text({
                x: 0,
                y: 0,
                origin: { x: "center", y: "center" }, align: "center",
                font: "bold 14px sans-serif",
                text: "x",
                fill: "white",
                zIndex: "front",
            }) 
        )
        debugWindow.addChild(close)
        close.bind('click tap', function () {
            space.scenes.unload('debug')
        })

        space.scenes.create('debug', function () {
            this.add(debugWindow)
        })

        space.setLoop(function () {
            debugInfo.text = [
                'x: '.concat(space.mouse.x),
                'y: '.concat(space.mouse.y),
                'correctCategory: '. concat(correctCategory),
                'endorseLabel: ' .concat(endorseLabel),
                'response: '. concat(response),
                'rt: '.concat(rt),
                'acc: '.concat(acc),
                '\n',
                'data:',
                params['results'].join('\n')
            ].join('\n')
        }).start()
    },

}