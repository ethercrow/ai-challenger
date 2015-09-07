import { EventEmitter } from 'events';
import ActionTypes from '../constants/ActionTypes';
import Dispatcher from '../dispatcher/AppDispatcher';

const initialData = {
    playing: false,
    current_turn: 0,
    max_turns: 0
};
let data = initialData;

const CONTROL_PANEL_CHANGE_EVENT = 'control_panel_change';

class ControlPanelStore extends EventEmitter {
    emitChange() {
        this.emit(CONTROL_PANEL_CHANGE_EVENT);
    }
    
    onChange(callback) {
        this.on(CONTROL_PANEL_CHANGE_EVENT, callback);
    }
    
    off(callback) {
        this.removeListener(CONTROL_PANEL_CHANGE_EVENT, callback);
    }
    
    reset(max_turns) {
        data.playing = false;
        data.current_turn = 0;
        data.max_turns = max_turns;
    }
    
    changeTurn(current_turn) {
        data.current_turn = current_turn;
    }
    
    startPlaying() {
        data.playing = true;
    }
    
    stopPlaying() {
        data.playing = false;
    }
    
    isPlaying() {
        return data.playing;
    }
    
    getCurrentTurn() {
        return data.current_turn;
    }
    
    getMaxTurns() {
        return data.max_turns;
    }
};

var _ControlPanelStore = new ControlPanelStore();
export default _ControlPanelStore;

_ControlPanelStore.dispatchToken = Dispatcher.register((action) => {
    switch(action.type) {
    case ActionTypes.CONTROL_PANEL_RESET:
        _ControlPanelStore.reset(action.data.max_turns);
        _ControlPanelStore.emitChange();
        break;
    
    case ActionTypes.CONTROL_PANEL_CHANGE_TURN:
        _ControlPanelStore.changeTurn(action.data.current_turn);
        _ControlPanelStore.emitChange();
        break;
    
    case ActionTypes.CONTROL_PANEL_START_PLAYING:
        _ControlPanelStore.startPlaying();
        _ControlPanelStore.emitChange();
        break;
    
    case ActionTypes.CONTROL_PANEL_STOP_PLAYING:
        _ControlPanelStore.stopPlaying();
        _ControlPanelStore.emitChange();
        break;
    
    default:
    }
});
