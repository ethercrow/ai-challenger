import React from 'react';
import ControlPanelActions from '../actions/ControlPanelActions';
import ControlPanelStore from '../stores/ControlPanelStore';

const player1_color = 'red';
const player2_color = 'blue';

class GridView extends React.Component {
    constructor(props) {
        super(props);
        
        this.canvas_width = 800;
        this.canvas_height = 600;
        
        this.canvas = undefined;
        this.animation_timer = undefined;
        this.advance_timer = undefined;
        this.keyup_handler = this._onKeyUp.bind(this);;
        
        this.turns = [];
        this.reset_store = false;
        this.start_playing = false;
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        this.canvas = document.getElementById('match-view');
        
        this.animation_timer = window.setInterval(this._drawFrame.bind(this), 100);
        
        ControlPanelStore.onChange(this._onChange);
        document.addEventListener('keyup', this.keyup_handler);
        
        this.start_playing = true;
    }
    
    componentWillUnmount() {
        ControlPanelStore.off(this._onChange);
        
        if(this.advance_timer) {
            window.clearInterval(this.advance_timer);
        }
        window.clearInterval(this.animation_timer);
        document.removeEventListener('keyup', this.keyup_handler);
    }
    
    render() {
        this.canvas_width = window.innerWidth;
        this.canvas_height = window.innerHeight;
        
        return (
            <canvas id="match-view" width={this.canvas_width} height={this.canvas_height}>
                Sorry, you browser doesn&#39;t support HTML5 canvas API!
            </canvas>
        );
    }
    
    _startPlaying() {
        this.advance_timer = window.setInterval(this._advanceTurn.bind(this), 1000);
    }
    
    _stopPlaying() {
        window.clearInterval(this.advance_timer);
        this.advance_timer = undefined;
    }
    
    _advanceTurn() {
        if(this._isLogReady() && this.state.current_turn < this.turns.length - 2) {
            ControlPanelActions.changeTurn(this.state.current_turn + 1);
        } else {
            if(this.state.current_turn = this.turns.length - 1 && this.state.playing) {
                ControlPanelActions.stopPlaying();
            }
        }
    }
    
    _drawFrame() {
        let context = this.canvas.getContext("2d");
        context.clearRect(0, 0, this.canvas_width, this.canvas_height);
        
        if(this._isLogReady()) {
            this._drawNormalFrame(context);
        } else {
            this._createTurns();
            this._drawLoadingFrame(context);
        }
    }
    
    _drawLoadingFrame(context) {
        this._drawTextBox(context, "Loading...", 0, 0, this.canvas_width, this.canvas_height);
    }
    
    _drawNormalFrame(context) {
        if(this.reset_store) {
            ControlPanelActions.reset(this.turns.length - 1);
            this.reset_store = false;
        }
        
        if(this.start_playing) {
            ControlPanelActions.startPlaying();
            this.start_playing = false;
        }
        
        const map = this._parseTurn(this.state.current_turn);
        const grid_params = this._computeGridParams(map);
        
        this._drawGrid(context, map, grid_params);
        this._drawPlayers(context, map, grid_params);
    }
    
    _computeGridParams(map) {
        const separator_width = 1;
        const padding = {
            x: 3,
            y: 3
        };
        
        const map_width = map[0].length;
        const map_height = map.length;
        
        const cell_max_geom = {
            width: Math.floor((this.canvas_width - 2*padding.x - map_width*separator_width)/map_width),
            height: Math.floor((this.canvas_height - 2*padding.y - map_height*separator_width)/map_height)
        };
        let cell = { width: 0, height: 0};
        if(cell_max_geom.width > cell_max_geom.height) {
            cell.width = cell_max_geom.height;
            cell.height = cell_max_geom.height;
        } else {
            cell.width = cell_max_geom.width;
            cell.height = cell_max_geom.width;
        }
        
        const grid = {
            width: map_width*cell.width + map_width*separator_width,
            height: map_height*cell.height + map_height*separator_width
        };
        
        const starting_point = {
            x: padding.x + Math.floor((this.canvas_width - 2*padding.x - grid.width)/2),
            y: padding.y + Math.floor((this.canvas_height - 2*padding.y - grid.height)/2)
        };
        
        return {
            separator_width: separator_width,
            
            starting_point: starting_point,
            grid: grid,
            cell: cell
        };
    }
    
    _drawGrid(context, map, grid_params) {
        context.strokeStyle = "green";
        context.lineWidth = grid_params.separator_width;
        
        let grid_path = new Path2D();
        
        context.save();
        
        context.beginPath();
        context.translate(grid_params.starting_point.x, grid_params.starting_point.y);
        context.moveTo(0, 0);
        context.lineTo(0, grid_params.grid.height);
        context.stroke();
        context.closePath();
        for(let i = 0; i < map[0].length; i++) {
            context.beginPath();
            context.translate(grid_params.cell.width + grid_params.separator_width, 0);
            context.moveTo(0, 0);
            context.lineTo(0, grid_params.grid.height);
            context.stroke();
            context.closePath();
        }
        
        context.restore();
        context.save();
        
        context.beginPath();
        context.translate(grid_params.starting_point.x, grid_params.starting_point.y);
        context.moveTo(0, 0);
        context.lineTo(grid_params.grid.width, 0);
        context.stroke();
        context.closePath();
        for(let i = 0; i < map[0].length; i++) {
            context.beginPath();
            context.translate(0, grid_params.cell.height + grid_params.separator_width);
            context.moveTo(0, 0);
            context.lineTo(grid_params.grid.width, 0);
            context.stroke();
            context.closePath();
        }
        
        context.restore();
    }
    
    _drawPlayers(context, map, grid_params) {
        for(let x = 0; x < map[0].length; x++) {
            for(let y = 0; y < map.length; y++) {
                switch(map[y][x]) {
                case 1:
                    this._drawPlayer(context, grid_params, x, y, player1_color);
                    break;
                
                case 2:
                    this._drawPlayer(context, grid_params, x, y, player2_color);
                    break;
                }
            }
        }
    }
    
    _drawPlayer(context, grid_params, x, y, color) {
        context.save();
        context.translate(grid_params.starting_point.x + x*(grid_params.separator_width + grid_params.cell.width),
        grid_params.starting_point.y + y*(grid_params.separator_width + grid_params.cell.height));
        
        context.beginPath();
        context.fillStyle = color;
        context.arc(Math.floor(grid_params.cell.width/2), Math.floor(grid_params.cell.height/2), 
                    Math.floor(grid_params.cell.width/2) - 2, 0, 2*Math.PI, true);
        context.fill();
        context.closePath();
        
        context.restore();
    }
    
    _drawTextBox(context, text, x, y, width, height) {
        context.fillStyle = "green";
        context.font = '48px monospace';
        context.textAlign = 'start';
        context.textBaseline = 'middle';
        
        const text_data = context.measureText(text);
        context.fillText(text, x + (width - text_data.width)/2, y + height/2, text_data.width);
    }
    
    _isLogReady() {
        return this.turns.length != 0;
    }
    
    _createTurns() {
        if(this.props.match != undefined && this.props.match.log != undefined) {
            this.turns = this.props.match.log.split('.');
            this.reset_store = true;
            this.start_playing = true;
        }
    }
    
    _parseTurn(n) {
        const map_str = this.turns[n].split('O')[0];
        const map_lines = map_str.split('\n');
        
        const first_line_idx = map_lines.findIndex((line) => { return line == "W"; }) + 1;
        const map_height = map_lines.reduce((height, line, index) => {
            if(index >= first_line_idx) {
                if(line.length != 0) {
                    return height + 1;
                } else {
                    return height;
                }
            } else {
                return height;
            }
        }, 0);
        
        const map_width = map_lines[first_line_idx].length;
        
        let map = [];
        for(let i = first_line_idx; i < first_line_idx + map_height; i++) {
            let line = [];
            for(let j = 0; j < map_lines[i].length; j++) {
                switch(map_lines[i][j]) {
                case '-':
                    line.push(0);
                    break;
                
                case '1':
                    line.push(1);
                    break;
                
                case '2':
                    line.push(2);
                    break;
                
                default:
                    console.error('ERROR: Unknown symbol encountered during the map parsing!');
                    line.push(undefined);
                }
            }
            map.push(line);
        }
        
        return map;
    }
    
    _onKeyUp(event) {
        switch(event.keyCode) {
        case 32:
            // Space pressed
            if(this.state.playing) {
                ControlPanelActions.stopPlaying();
            } else {
                ControlPanelActions.startPlaying();
            }
            break;
        
        case 39:
            // Left arrow
            if(event.shiftKey) {
                if(this.state.current_turn + 10 <= this.turns.length - 2) {
                    ControlPanelActions.changeTurn(this.state.current_turn + 10);
                }
            } else {
                if(this.state.current_turn < this.turns.length - 2) {
                    ControlPanelActions.changeTurn(this.state.current_turn + 1);
                }
            }
            break;
        
        case 37:
            // Right arrow
            if(event.shiftKey) {
                if(this.state.current_turn - 10 >= 0) {
                    ControlPanelActions.changeTurn(this.state.current_turn - 10);
                }
            } else {
                if(this.state.current_turn != 0) {
                    ControlPanelActions.changeTurn(this.state.current_turn - 1);
                }
            }
            break;
        
        default:
        }
    }
    
    _resolveState() {
        return {
            playing: ControlPanelStore.isPlaying(),
            current_turn: ControlPanelStore.getCurrentTurn()
        };
    }
    
    _onChange() {
        let new_state = this._resolveState();
        
        if(this.state.playing != new_state.playing) {
            if(new_state.playing) {
                this._startPlaying();
            } else {
                this._stopPlaying();
            }
        }
        
        this.setState(new_state);
    }
};

export default GridView;
