const AudioContext = window.AudioContext || window.webkitAudioContext;
const audioCtx = new AudioContext();

export default class Audio {
  constructor() {
    this.attackTime = 0.01;
    this.releaseTime = 1;
  }

  playNote(noteNumber) {
    const freq = Math.pow(2, (noteNumber - 69) / 12) * 440;
    const env = this._generateEnvelope();
    const osc = audioCtx.createOscillator();
    osc.type = "sine";
    osc.frequency.value = freq;

    env.connect(audioCtx.destination);
    osc.connect(env);
    osc.start();
    osc.stop(audioCtx.currentTime + this.envTime);
  }

  get envTime() {
    return this.attackTime + this.releaseTime;
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
