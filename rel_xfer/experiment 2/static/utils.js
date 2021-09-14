// redirects user to the next template that's provided in the message response
async function start(name) {
    localStorage.setItem('name', name)

    // Send participant data from initial survey
    const  options = {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({
            'message': 'start',
            'name': name,
        })
    }

    const resp = await fetch('/start', options)
    const data = await resp.json()
    localStorage.setItem('subject_id', data['subject_id'])
    localStorage.setItem('condition', data['condition'])
    window.location = data['message']
}



// redirects user to the next template that's provided in the message response
async function next(experiment, subject_id, condition, results) {
    console.log('experiment complete')

    // v-- ignore all that --v

    // // Send participant data from initial survey
    // const  options = {
    //     method: 'POST',
    //     headers: {
    //         'Content-Type': 'application/json'
    //     },
    //     body: JSON.stringify({
    //         'message': 'next',
    //         'subject_id': localStorage.getItem('subject_id'),
    //         'condition': condition,
    //         'experiment': experiment,
    //         'results': results,
    //         'name': localStorage.getItem('name')
    //     })
    // }

    // const resp = await fetch('/next', options)
    // const data = await resp.json()
    // console.log(data['condition'])
    // localStorage.setItem('subject_id', data['subject_id'])
    // localStorage.setItem('condition', data['condition'])
    // localStorage.setItem('check',data['check'])
    // window.location = data['message']
}



// You'll need these in your experiment script
function get_subject_id() {
    return 123 // <-- this is a test
}
function get_condition() {
    return 1 // <-- this is also just a test
}



// return debriefing forms for relevant experiment
async function debriefing() {
    const options = {
        method: 'POST',
        headers: {
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({
            'message': 'debrief',
            'subject_id': localStorage.getItem('subject_id'),
            'check': localStorage.getItem('check'),
        })
    }
    const resp = await fetch('/debriefing', options)
    const data = await resp.json()
    // console.log(data)
    // data = {'message': 'recieved', 'debriefing_forms': {'basicExp2': 'This is the <b>briefing</b> form for a project', 'transl1': 'This is the other debriefing form for a project', 'transl2': 'This is the other debriefing form for a project', 'basicExp': 'This is the other debriefing form for a project'}}
    return data
}

function viewData(data) {
    window.open('data:text/plain;charset=utf-8,' + escape(data));
}
