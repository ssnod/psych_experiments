var clCo = {

    start: function(params, next) {
        params['results'].push(['experiment','phase','trial','block','lstimId','rstimId','category','response','accuracy','rt'])

        /*   initialize   */
        space = oCanvas.create({
            canvas: clCo.initCanvas(),
            background: 'rgba(228,216,232,1)',
            // origin: { x: "center", y: "center" },
        })
        cx = space.canvasElement.width / 2
        cy = space.canvasElement.height / 2
        
        clCo.initElements(params)
        clCo.initDebugWindow(params)
        space.scenes.load('debug')

        x = new Date().getTime()
        experiment ='relx'
        phase = 'initialclCo'
        rt = 0
        response = null
        acc = null
        trial = 0
        block = 0
        block_order = clCo.generateBlockOrder(params['objects'], params['labels']) // <-- initialize the first block order (you'll do this every block)

        correctCategory = block_order[trial][1]
        clCo.initLStim(block_order[trial][0])
        clCo.initRStim(block_order[trial][2])

        setTimeout( function(){
            space.scenes.load('selection')

        }, 500)

        /*   define event logic   */
        for(category of params['categories']){
            responseBtns[category].btn.bind('click tap', function () {
                rt = new Date().getTime() - x

                response = this.category
                acc = response == correctCategory

                if (acc == true) {
                    feedbackTxt.text = 'Correct! Both of these are a '.concat(params['btnLabels'][correctCategory])
                } else {
                    feedbackTxt.text = 'Incorrect... Both of these are a '.concat(params['btnLabels'][correctCategory], ".\n (You responded ", response, ")")
                }

                space.scenes.unload('selection')
                space.scenes.load('feedback')

                setTimeout( function () {
                    
                    space.scenes.load('continue')   

                }, 500)

            })

        }

        continueBtn.bind('click tap', function () {
            space.scenes.unload('continue')
            space.scenes.unload('feedback')

            stimulusR.remove()
            stimulusL.remove()

            params['results'].push([experiment, phase, trial, block, block_order[trial][0].split('/').slice(-1)[0].split('.')[0],block_order[trial][2].split('/').slice(-1)[0].split('.')[0], correctCategory, response, acc, rt]) // pulled this variable from the main template
            trial += 1
            
            if (trial == block_order.length) {
                trial = 0
                block += 1
                if (block == params['n_blocks']) {
                    space.destroy(space)
                    document.getElementById('main').innerHTML = ''
                    next()
                } else {
                    block_order = clCo.generateBlockOrder(params['objects'], params['labels'])
                }
            }

            setTimeout( function () {
                setTimeout( function(){
                     space.scenes.load('selection')

                }, 500)
                correctCategory = block_order[trial][1]
                clCo.initLStim(block_order[trial][0])
                clCo.initRStim(block_order[trial][2])
                x = new Date().getTime()
            }, 200)

        })

        console.log('started clCo')
    },


    initElements: function (params) {
        centerX = space.canvasElement.width / 2
        centerY = space.canvasElement.height / 2

        /*   Response Phase of Trial   */
        responseBtns = {}
        btnCompression = .4
        for (let [index, category] of params['categories'].entries()) {
            let xpos = ((space.canvasElement.width / params['categories'].length) * index)  +  ((space.canvasElement.width / params['categories'].length) / 2)
            responseBtns[category] = {
                'category': category,
                'btnLabel': params['btnLabels'][category],
            }

            responseBtns[category].btn = space.display.rectangle({
                x: xpos + (((space.canvasElement.width / 2) - xpos) * btnCompression),
                y: centerY + 200,
                origin: { x: "center", y: "center" },// align: 'center',
                width: 160,
                height: 60,
                fill: "rgba(250,246,186,1)",
                stroke: "2px #000",
                join: "round",
                category: category, // <-- CAREFUL! This is a custom added property to the oCanvas Base object; if they decide to ever include it, this will overwrite whatever function they decide the 'category' property should have (though low odds of that happening)
            })
            responseBtns[category].btn.addChild(
                space.display.text({
                    x: 0,
                    y: 0,
                    origin: { x: "center", y: "center" },// align: 'center',
                    font: "bold 25px sans-serif",
                    text: responseBtns[category]['btnLabel'],
                    fill: "rgba(0,0,0,1)",
                    zIndex: "front",
                })
            )
        }

        instructionsTxt = space.display.text({
            x: centerX,
            y: centerY + 100,
            origin: { x: "center", y: "center" }, align: 'center',
            align: "center",
            font: "bold 25px sans-serif",
            text: "Both of these arrangements belong to the same category.\nClick a button to select the correct category.",
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
                y: cy + 120,
                origin: { x: "center", y: "top" },
                font: "bold 30px sans-serif",
                text: "Feedback Message",
                fill: "#000000"
            });
        } else {
            feedbackTxt = space.display.text({
                x: cx,
                y: cy,
                origin: { x: "center", y: "top" },
                font: "bold 30px sans-serif",
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
            fill: "rgba(250,246,186,1)",
            stroke: "2px #000",
            join: "round",
        })
        continueBtn.addChild(
            space.display.text({
                x: 0,
                y: 0,
                origin: { x: "center", y: "center" }, align: 'center',
                align: "center",
                font: "bold 25px sans-serif",
                text: "Next Trial",
                fill: "rgba(0,0,0,1)",
                // zIndex: "front",
            })
        )
        space.scenes.create('continue', function () {
            this.add(continueBtn)
        })
    },


    initLStim: function (image) {
        centerX = space.canvasElement.width / 2
        centerY = space.canvasElement.height / 2

        stimulusL = space.display.image({
            x: centerX - 200,
            y: centerY - 100,
            origin: { x: "center", y: "center" }, align: 'center',
            image: image,
        })
        space.addChild(stimulusL)
    },

    initRStim: function (image) {
        centerX = space.canvasElement.width / 2
        centerY = space.canvasElement.height / 2

        stimulusR = space.display.image({
            x: centerX + 200,
            y: centerY - 100,
            origin: { x: "center", y: "center" }, align: 'center',
            image: image,
        })
        space.addChild(stimulusR)
    },

    createRandomOrder: function (objects, labels){
         // get randomized order of items
        let order = []

        idx = clCo.shuffle([...Array(labels.length).keys()]) // from ben @ https://stackoverflow.com/a/10050831

        for (let i of idx) {
            order.push([objects[i], labels[i]])
        }

        return order

    },

    createPairs: function(order){
        // create same category pairs
        let besodItems = []
        let makifItems = []
        let tolarItems = []

        // subset items from each category
        for (let i = 0; i < order.length; i++){
            if(order[i][1] == "Besod"){
                besodItems.push(order[i])
            }else if(order[i][1] == "Makif"){
                makifItems.push(order[i])
            }else if(order[i][1] == "Tolar"){
                tolarItems.push(order[i])
            }
        }
        
        pairsList = [besodItems[0].concat(besodItems[1]), besodItems[2].concat(besodItems[3]), besodItems[4].concat(besodItems[5]), besodItems[6].concat(besodItems[7]), makifItems[0].concat(makifItems[1]), makifItems[2].concat(makifItems[3]), makifItems[4].concat(makifItems[5]),makifItems[6].concat(makifItems[7]),tolarItems[0].concat(tolarItems[1]), tolarItems[2].concat(tolarItems[3]), tolarItems[4].concat(tolarItems[5]),tolarItems[6].concat(tolarItems[7])]
        console.log(pairsList)

        return pairsList
    },

    generateBlockOrder: function (objects, labels) {

       rndmOrder = clCo.createRandomOrder(objects, labels)

       pairList = clCo.createPairs(rndmOrder)
       

    // create training block
        let block = []

        idx = clCo.shuffle([...Array(pairList.length).keys()]) // from ben @ https://stackoverflow.com/a/10050831

        for (let i of idx) {
            block.push(pairList[i])
        }

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