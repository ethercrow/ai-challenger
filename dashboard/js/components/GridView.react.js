import React from 'react';
import ControlPanelActions from '../actions/ControlPanelActions';
import ControlPanelStore from '../stores/ControlPanelStore';

import LogParser from '../utils/LogParser';

const _gridview_params = {
    player_colors: ['red', 'blue'],
    delays: {
        animation: 50,
        advance: 1000,
        transition: 1000/100
    },
    max_transition_counter: 40
};

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
        
        this.transition_counter = 0;
        this.transition_timer = undefined;
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        this.canvas = document.getElementById('match-view');
        
        this.animation_timer = window.setInterval(this._drawFrame.bind(this), _gridview_params.delays.animation);
        
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
        this.advance_timer = window.setInterval(this._advanceTurn.bind(this), _gridview_params.delays.advance);
    }
    
    _stopPlaying() {
        window.clearInterval(this.advance_timer);
        this.advance_timer = undefined;
    }
    
    _advanceTurn() {
        if(this._isLogReady() && this.state.current_turn < this.turns.length - 1) {
            ControlPanelActions.changeTurn(this.state.current_turn + 1);
        } else {
            if(this.state.current_turn >= this.turns.length - 1 && this.state.playing) {
                ControlPanelActions.stopPlaying();
            }
        }
    }
    
    _startTransition() {
        if(this.transition_timer) {
            this._stopTransition();
        }
        
        this.transition_timer = window.setInterval(this._advanceTransition.bind(this), _gridview_params.delays.transition);
        this.transition_counter = _gridview_params.max_transition_counter;
    }
    
    _stopTransition() {
        if(this.transition_timer) {
            window.clearInterval(this.transition_timer);
            this.transition_counter = 0;
        }
    }
    
    _advanceTransition() {
        if(this.transition_counter == 0) {
            this._stopTransition();
        } else {
            this.transition_counter--;
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
            ControlPanelActions.reset(this.turns.length);
            this.reset_store = false;
        }
        
        if(this.start_playing) {
            ControlPanelActions.startPlaying();
            this.start_playing = false;
        }
        
        let prev_map = undefined;
        if(this.state.current_turn != 0) {
            prev_map = LogParser.parseTurn(this.turns[this.state.current_turn - 1]).map;
        }
        
        const map_and_moves = LogParser.parseTurn(this.turns[this.state.current_turn]);
        const map = map_and_moves.map;
        const grid_params = this._computeGridParams(map);
        
        this._drawGrid(context, map, grid_params);
        this._drawPlayers(context, map, prev_map, grid_params);
        this._drawMoves(context, grid_params, map_and_moves.commands);
        this._drawPlayersZones(context, grid_params, map);
    }
    
    _computeGridParams(map) {
        const separator_width = 1;
        const padding = {
            x: 3,
            y: 3
        };
        
        const player_zone_width = Math.floor(this.canvas_width*0.1);
        const grid_canvas_width = this.canvas_width - 2*player_zone_width;
        
        const map_width = map[0].length;
        const map_height = map.length;
        
        const cell_max_geom = {
            width: Math.floor((grid_canvas_width - 2*padding.x - map_width*separator_width)/map_width),
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
            x: player_zone_width + padding.x + Math.floor((grid_canvas_width - 2*padding.x - grid.width)/2),
            y: padding.y + Math.floor((this.canvas_height - 2*padding.y - grid.height)/2)
        };
        
        return {
            separator_width: separator_width,
            
            player_zone: {
                width: player_zone_width,
                height: this.canvas_height
            },
            
            starting_point: starting_point,
            grid: grid,
            cell: cell
        };
    }
    
    _drawGrid(context, map, grid_params) {
        context.strokeStyle = "green";
        context.lineWidth = grid_params.separator_width;
        
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
    
    _drawPlayers(context, map, prev_map, grid_params) {
        for(let x = 0; x < map[0].length; x++) {
            for(let y = 0; y < map.length; y++) {
                if(map[y][x] != 0) {
                    if(prev_map == undefined || prev_map[y][x] == 0) {
                        const color = _gridview_params.player_colors[map[y][x] - 1];
                        const transition = 'appear';
                        this._drawPlayer(context, grid_params, x, y, color, transition);
                    } else {
                        if(prev_map[y][x] != map[y][x]) {
                            let color = _gridview_params.player_colors[prev_map[y][x] - 1];
                            let transition = 'disappear';
                            this._drawPlayer(context, grid_params, x, y, color, transition);
                            
                            color = _gridview_params.player_colors[map[y][x] - 1];
                            transition = 'appear';
                            this._drawPlayer(context, grid_params, x, y, color, transition);
                        } else {
                            let color = _gridview_params.player_colors[map[y][x] - 1];
                            let transition = 'none';
                            this._drawPlayer(context, grid_params, x, y, color, transition);
                        }
                    }
                } else {
                    if(prev_map != undefined && prev_map[y][x] != 0) {
                        let color = _gridview_params.player_colors[prev_map[y][x] - 1];
                        let transition = 'disappear';
                        this._drawPlayer(context, grid_params, x, y, color, transition);
                    }
                }
            }
        }
    }
    
    _drawPlayer(context, grid_params, x, y, color, transition) {
        const full_size = Math.floor(grid_params.cell.width/2) - 2;
        const step = full_size/_gridview_params.max_transition_counter;
        let size = full_size;
        switch(transition) {
        case 'appear':
            size = (_gridview_params.max_transition_counter - this.transition_counter)*step;
            break;
        
        case 'disappear':
            size = this.transition_counter*step;
            break;
        
        default:
        }
        
        context.save();
        context.translate(grid_params.starting_point.x + x*(grid_params.separator_width + grid_params.cell.width),
                          grid_params.starting_point.y + y*(grid_params.separator_width + grid_params.cell.height));
        
        context.beginPath();
        context.fillStyle = color;
        context.arc(Math.floor(grid_params.cell.width/2), Math.floor(grid_params.cell.height/2), size, 0, 2*Math.PI, true);
        context.fill();
        context.closePath();
        
        context.restore();
    }
    
    _drawMoves(context, grid_params, commands) {
        for(let player = 0; player < 2; player++) {
            context.strokeStyle = _gridview_params.player_colors[player];
            context.lineWidth = 3;
            
            const padding = 2*player;
            commands[player].forEach((cmd) => {
                context.save();
                context.translate(grid_params.starting_point.x + cmd.x*(grid_params.separator_width + grid_params.cell.width) + padding,
                                  grid_params.starting_point.y + cmd.y*(grid_params.separator_width + grid_params.cell.height) + padding);
                
                context.strokeRect(0, 0, grid_params.cell.width - 2*padding, grid_params.cell.height - 2*padding);
                
                context.restore();
            });
        }
    }
    
    _drawPlayersZones(context, grid_params, map) {
        const scores = this._computePlayersScores(map);
        const max_score = map.length*map[0].length;
        
        this._drawPlayerZone(context, 0, scores[0] >= scores[1], scores[0], max_score, 
                             0, 0, grid_params.player_zone.width, grid_params.player_zone.height);
        this._drawPlayerZone(context, 1, scores[1] >= scores[0], scores[1], max_score, 
                             this.canvas_width - grid_params.player_zone.width, 0, 
                             grid_params.player_zone.width, grid_params.player_zone.height);
    }
    
    _drawPlayerZone(context, player, is_winning, score, max_score, x, y, width, height) {
        let player_name = 'none';
        switch(player) {
        case 0:
            player_name = this.props.bot1_name;
            break;
        
        case 1:
            player_name = this.props.bot2_name;
            break;
        }
        
        this._drawTextBox(context, player_name, x, y + Math.floor(0.8*height), width, Math.floor(0.1*height));
        this._drawTextBox(context, score, x, y + Math.floor(0.9*height), width, Math.floor(0.1*height));
        
        context.strokeStyle = _gridview_params.player_colors[player];
        context.fillStyle = _gridview_params.player_colors[player];
        context.lineWidth = 3;
        
        context.beginPath();
        context.moveTo(x + 3, y + Math.floor(0.8*height) - 3);
        context.lineTo(x + width - 6, y + Math.floor(0.8*height) - 3);
        context.stroke();
        context.closePath();
        
        const column_max_height = Math.floor(0.6*height);
        const column_height = Math.floor(score*column_max_height/max_score);
        context.fillRect(x + 6, y + Math.floor(0.8*height) - 3 - column_height, width - 15, column_height);
        
        this._drawPlayerFace(context, is_winning, x + 6, y + Math.floor(0.8*height) - 3 - column_height - Math.floor(0.1*height), 
                             width - 15, Math.floor(0.1*height));
    }
    
    _drawPlayerFace(context, is_winning, x, y, width, height) {
        const radius = (width , height)?Math.floor(width/2):Math.floor(height/2);
        
        context.beginPath();
        context.arc(x + width - radius, y + height - radius, radius, 0, 2*Math.PI, true);
        context.stroke();
        context.closePath();
        
        context.beginPath();
        context.arc(x + width - 0.70*2*radius, y + height - 0.65*2*radius, 0.15*radius, 0, 2*Math.PI, true);
        context.fill();
        context.closePath();
        
        context.beginPath();
        context.arc(x + width - 0.30*2*radius, y + height - 0.65*2*radius, 0.15*radius, 0, 2*Math.PI, true);
        context.fill();
        context.closePath();
        
        if(is_winning) {
            context.beginPath();
            context.arc(x + width - radius, y + height - 0.65*2*radius, 0.9*radius, 3/4*Math.PI, 1/4*Math.PI, true);
            context.stroke();
            context.closePath();
        } else {
            context.beginPath();
            context.arc(x + width - radius, y + height + 0.3*radius, 0.9*radius, 7/4*Math.PI, 5/4*Math.PI, true);
            context.stroke();
            context.closePath();
        }
    }
    
    _drawTextBox(context, text, x, y, width, height) {
        context.fillStyle = "green";
        context.font = '48px monospace';
        context.textAlign = 'start';
        context.textBaseline = 'middle';
        
        const text_data = context.measureText(text);
        context.fillText(text, x + (width - text_data.width)/2, y + height/2, text_data.width);
    }
    
    _computePlayersScores(map) {
        let result = [0, 0];
        
        for(let x = 0; x < map[0].length; x++) {
            for(let y = 0; y < map.length; y++) {
                switch(map[y][x]) {
                case 1:
                    result[0] += 1;
                    break;
                
                case 2:
                    result[1] += 1;
                    break;
                }
            }
        }
        
        return result;
    }
    
    _isLogReady() {
        return this.turns.length != 0;
    }
    
    _createTurns() {
        if(this.props.match != undefined && this.props.match.log != undefined) {
            this.turns = LogParser.separateTurns(this.props.match.log);
            this.reset_store = true;
            this.start_playing = true;
        }
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
                if(this.state.current_turn + 10 <= this.turns.length - 1) {
                    ControlPanelActions.changeTurn(this.state.current_turn + 10);
                }
            } else {
                if(this.state.current_turn < this.turns.length - 1) {
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
        
        if(this.state.current_turn < new_state.current_turn) {
            this._startTransition();
        }
        
        this.setState(new_state);
    }
};

export default GridView;
