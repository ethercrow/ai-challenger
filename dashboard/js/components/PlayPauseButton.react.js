import React from 'react';
import ControlPanelActions from '../actions/ControlPanelActions';
import ControlPanelStore from '../stores/ControlPanelStore';

class PlayPauseButton extends React.Component {
    constructor(props) {
        super(props);
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        ControlPanelStore.onChange(this._onChange);
    }
    
    componentWillUnmount() {
        ControlPanelStore.off(this._onChange);
    }
    
    render() {
        if(this.state.playing) {
            return (<img id="play-pause-button" src="images/pause.png" alt="||" onClick={this._onPause.bind(this)}/>);
        } else {
            return (<img id="play-pause-button" src="images/play.png" alt=">" onClick={this._onPlay.bind(this)}/>);
        }
    }
    
    _onPlay() {
        ControlPanelActions.startPlaying();
    }
    
    _onPause() {
        ControlPanelActions.stopPlaying();
    }
    
    _resolveState() {
        return {
            playing: ControlPanelStore.isPlaying()
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default PlayPauseButton;
