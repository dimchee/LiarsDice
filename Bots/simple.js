const { inspect } = require('util');
const log = x => console.log(inspect(x, {
    depth: null,
    colorize: true
}))

const net = require('net')
const soc = net.createConnection({port: 8888})
const send = server => move => soc.write(JSON.stringify({
    message_id: server.message_id,
    move: move,
}))

function delay(time) {
  return new Promise(resolve => setTimeout(resolve, time));
} 

soc.on('connect', () => {
    soc.write(JSON.stringify({
        name: process.argv.slice(2)[0]
    }))
})
soc.on('data', async data => {
    const msg = JSON.parse(data.toString());
    let respond = send(msg)
    let pass = () => respond("pass")
    let challenge = () => respond("challenge")
    let bid = (x) => respond([x.value, x.count])

    // log(msg);
    log(msg)
    if(msg.other_hands[0][0] == "yourself") {
        bid({ count: 5, value: 4 })
    } else {
        pass()
    }
    // await delay(1000)
    // respond("challenge")
})
