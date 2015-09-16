import React from 'react';
import ControlPanelActions from '../actions/ControlPanelActions';
import ControlPanelStore from '../stores/ControlPanelStore';

class ProgressBar extends React.Component {
    constructor(props) {
        super(props);
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        ControlPanelStore.onChange(this._onChange);
        this._drawFrame();
    }
    
    componentWillUnmount() {
        ControlPanelStore.off(this._onChange);
    }
    
    componentDidUpdate() {
        this._drawFrame();
    }
    
    render() {
        if(this.state.cursor_drag_in_progress) {
            return (
                <canvas id="progress-bar" className="cursor_drag_in_progress" width="600" height="50" 
                    onMouseDown={this._onMouseDown.bind(this)}
                    onMouseMove={this._onMouseMove.bind(this)}
                    onMouseUp={this._onMouseUp.bind(this)}></canvas>
            );
        } else {
            return (
                <canvas id="progress-bar" width="600" height="50" 
                    onMouseDown={this._onMouseDown.bind(this)}
                    onMouseMove={this._onMouseMove.bind(this)}
                    onMouseUp={this._onMouseUp.bind(this)}></canvas>
            );
        }
    }
    
    _onMouseDown(event) {
        const pos = this._getMousePos(event);
        
        if(pos.x >= 10 && pos.x <= 590) {
            ControlPanelActions.changeTurn(Math.floor((pos.x - 10)*this.state.max_turns/580));
        }
        
        this.setState({ cursor_drag_in_progress: true });
    }
    
    _onMouseMove(event) {
        if(this.state.cursor_drag_in_progress) {
            const pos = this._getMousePos(event);
        
            if(pos.x >= 10 && pos.x <= 590) {
                ControlPanelActions.changeTurn(Math.floor((pos.x - 10)*this.state.max_turns/580));
            }
        }
    }
    
    _onMouseUp(event) {
        this.setState({ cursor_drag_in_progress: false });
    }
    
    _getMousePos(event) {
        let canvas = document.getElementById('progress-bar');
        let rect = canvas.getBoundingClientRect();
        
        return {
            x: 600*(event.clientX - rect.left)/rect.width,
            y: 50*(event.clientY - rect.top)/rect.height
        };
    }
    
    _drawFrame() {
        let canvas = document.getElementById('progress-bar');
        let context = canvas.getContext("2d");
        context.clearRect(0, 0, 600, 50);
    
        context.strokeStyle = "green";
        context.lineWidth = 2;
    
        let baseline = new Path2D();
        baseline.moveTo(10, 25);
        baseline.lineTo(590, 25);
        context.stroke(baseline);
    
        const cursor_x = (this.state.max_turns != 0)?(580*(this.state.current_turn))/(this.state.max_turns - 1):0;
        let cursor = new Path2D();
        cursor.moveTo(10 + cursor_x, 5);
        cursor.lineTo(10 + cursor_x, 45);
        context.stroke(cursor);
    }
    
    _resolveState() {
        return {
            current_turn: ControlPanelStore.getCurrentTurn(),
            max_turns: ControlPanelStore.getMaxTurns(),
            cursor_drag_in_progress: this.state?this.state.cursor_drag_in_progress:false
        };
    }
    
    _onChange() {
        this.setState(this._resolveState());
    }
};

export default ProgressBar;
