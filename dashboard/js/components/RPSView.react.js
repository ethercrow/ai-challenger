import React from 'react';
import ControlPanelActions from '../actions/ControlPanelActions';
import ControlPanelStore from '../stores/ControlPanelStore';

const images_ids = {
    ROCK_IMAGE: 0,
    PAPER_IMAGE: 1,
    SCISSORS_IMAGE: 2
};

let image_resources = {
    images: [undefined, undefined, undefined],
    image_loaded_flags: [false, false, false]
};

class RPSView extends React.Component {
    constructor(props) {
        super(props);
        
        this.canvas = undefined;
        this.animation_timer = undefined;
        this.advance_timer = undefined;
        
        this.turns = [];
        this.current_turn = 0;
        this.reset_store = false;
        this.start_playing = false;
        
        this.state = this._resolveState();
        this._onChange = this._onChange.bind(this);
    }
    
    componentDidMount() {
        this.canvas = document.getElementById('match-view');
        this._generateTurns();
        this.reset_store = true;
        
        this._startLoadingImages();
        this.animation_timer = window.setInterval(this._drawFrame.bind(this), 100);
        
        ControlPanelStore.onChange(this._onChange);
        document.addEventListener('keyup', this._onKeyUp.bind(this));
        
        this.start_playing = true;
    }
    
    componentWillUnmount() {
        ControlPanelStore.off(this._onChange);
        
        if(this.advance_timer) {
            window.clearInterval(this.advance_timer);
        }
        window.clearInterval(this.animation_timer);
        document.removeEventListener('keyup', this._onKeyUp.bind(this));
    }
    
    render() {
        return (
            <canvas id="match-view" width="800" height="550">
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
        if(this._areImagesLoaded() && this.current_turn < this.turns.length - 1) {
            this.current_turn += 1;
            ControlPanelActions.changeTurn(this.current_turn);
        } else {
            if(this.current_turn = this.turns.length - 1 && this.state.playing) {
                ControlPanelActions.stopPlaying();
            }
        }
    }
    
    _drawFrame() {
        if(this.reset_store) {
            ControlPanelActions.reset(this.turns.length);
            this.reset_store = false;
        }
        
        if(this.start_playing) {
            ControlPanelActions.startPlaying();
            this.start_playing = false;
        }
        
        let context = this.canvas.getContext("2d");
        context.clearRect(0, 0, 800, 600);
        
        if(this._areImagesLoaded()) {
            this._drawNormalFrame(context);
        } else {
            this._drawLoadingFrame(context);
        }
    }
    
    _drawLoadingFrame(context) {
        context.strokeStyle = "green";
        context.lineWidth = 2;
        context.strokeRect(100, 290, 600, 20);
        
        const num_images = image_resources.image_loaded_flags.length;
        const num_loaded_images = image_resources.image_loaded_flags.reduce((num_images, flag) => {
            if(flag) return ++num_images;
            else return num_images;
        }, 0);
        
        const rect_width = (596*num_loaded_images)/num_images;
        
        context.fillStyle = "green";
        context.fillRect(102, 292, rect_width, 16);
    }
    
    _drawNormalFrame(context) {
        const turn = this._getTurn(this.current_turn);
        
        context.fillStyle = "green";
        context.font = '48px monospace';
        context.textAlign = 'start';
        context.textBaseline = 'middle';
        
        this._drawTextBox(context, this.props.bot1_name, 125, 100, 150, 100);
        this._drawMoveImage(context, turn.moves[0], 125, 200);
        this._drawTextBox(context, turn.scores[0], 125, 400, 150, 100);
        
        this._drawTextBox(context, this.props.bot2_name, 525, 100, 150, 100);
        this._drawMoveImage(context, turn.moves[1], 525, 200);
        this._drawTextBox(context, turn.scores[1], 525, 400, 150, 100);
    }
    
    _drawMoveImage(context, move, x, y) {
        switch(move) {
        case 'R':
            context.drawImage(image_resources.images[images_ids.ROCK_IMAGE], x, y);
            break;
        
        case 'P':
            context.drawImage(image_resources.images[images_ids.PAPER_IMAGE], x, y);
            break;
        
        case 'S':
            context.drawImage(image_resources.images[images_ids.SCISSORS_IMAGE], x, y);
            break;
        
        default:
            console.error('Invalid move!');
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
    
    _generateTurns() {
        this.turns = this.props.match.log.split('\n');
        this.current_turn = 0;
    }
    
    _getTurn(n) {
        let turn_components = this.turns[n].split(' ');
        
        return {
            scores: [Number.parseInt(turn_components[0]), Number.parseInt(turn_components[1])],
            moves: [turn_components[2], turn_components[3]]
        };
    }
    
    _areImagesLoaded() {
        return image_resources.image_loaded_flags.reduce((result, flag) => {
            if(!flag) {
                return false;
            } else {
                return result;
            }
        }, true);
    }
    
    _startLoadingImages() {
        this._startLoadingImage(images_ids.ROCK_IMAGE, "images/rock.jpg");
        this._startLoadingImage(images_ids.PAPER_IMAGE, "images/paper.jpg");
        this._startLoadingImage(images_ids.SCISSORS_IMAGE, "images/scissors.jpg");
        
        /* window.setTimeout(this._startLoadingImage.bind(this), 3000, images_ids.ROCK_IMAGE, "images/rock.jpg");
        window.setTimeout(this._startLoadingImage.bind(this), 6000, images_ids.PAPER_IMAGE, "images/paper.jpg");
        window.setTimeout(this._startLoadingImage.bind(this), 9000, images_ids.SCISSORS_IMAGE, "images/scissors.jpg"); */
    }
    
    _startLoadingImage(image_id, image_url) {
        if(!image_resources.image_loaded_flags[image_id]) {
            image_resources.images[image_id] = new Image();
            image_resources.images[image_id].addEventListener("load", function() {
                image_resources.image_loaded_flags[image_id] = true;
            });
            image_resources.images[image_id].src = image_url;
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
                if(this.current_turn + 10 <= this.turns.length - 1) {
                    ControlPanelActions.changeTurn(this.current_turn + 10);
                }
            } else {
                if(this.current_turn < this.turns.length - 1) {
                    ControlPanelActions.changeTurn(this.current_turn + 1);
                }
            }
            break;
        
        case 37:
            // Right arrow
            if(event.shiftKey) {
                if(this.current_turn - 10 >= 0) {
                    ControlPanelActions.changeTurn(this.current_turn - 10);
                }
            } else {
                if(this.current_turn != 0) {
                    ControlPanelActions.changeTurn(this.current_turn - 1);
                }
            }
            break;
        
        default:
        }
    }
    
    _resolveState() {
        return {
            playing: ControlPanelStore.isPlaying()
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
        
        this.setState(this._resolveState());
        this.current_turn = ControlPanelStore.getCurrentTurn();
    }
};

export default RPSView;
