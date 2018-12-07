const AudioContext = window.AudioContext || window.webkitAudioContext;
const audioCtx = new AudioContext();
const mixer = audioCtx.createGain();
const baseGain = 0.9;
mixer.gain.setValueAtTime(baseGain, audioCtx.currentTime);
mixer.connect(audioCtx.destination);
let noteCount = 0;

export default class Audio {
  constructor() {
    this.attackTime = 0.01;
    this.releaseTime = 1;
  }

  playNote(noteNumber) {
    const freq = Math.pow(2, (noteNumber - 69) / 12) * 440;
    const env = this._generateEnvelope();
    const osc = audioCtx.createOscillator();
    osc.type = "triangle";
    osc.frequency.value = freq;

    env.connect(mixer);
    osc.connect(env);
    osc.start();
    this.addNote();
    osc.stop(audioCtx.currentTime + this.envTime);
    setTimeout(this.removeNote, this.envTime * 1000);
  }

  get envTime() {
    return this.attackTime + this.releaseTime;
  }

  addNote() {
    noteCount += 1;
    this.rebalanceMixer();
  }

  removeNote() {
    noteCount -= 1;
    this.rebalanceMixer();
  }

  rebalanceMixer() {
    const volume = baseGain / (noteCount * 0.8);
    mixer.gain.setValueAtTime(volume, audioCtx.currentTime);
  }

  _generateEnvelope() {
    const off = 0.0001;
    const env = audioCtx.createGain();
    env.gain.cancelScheduledValues(audioCtx.currentTime);
    env.gain.setValueAtTime(0, audioCtx.currentTime);
    env.gain.linearRampToValueAtTime(1, audioCtx.currentTime + this.attackTime);
    env.gain.exponentialRampToValueAtTime(
      off,
      audioCtx.currentTime + this.envTime
    );

    return env;
  }
}
