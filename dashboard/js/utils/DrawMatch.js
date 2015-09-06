const initialData = {
    canvas: undefined,
    animation_timer: undefined,
    
    match: undefined,
    bot1_name: '',
    bot2_name: ''
};

const images_ids = {
    ROCK_IMAGE: 0,
    PAPER_IMAGE: 1,
    SCISSORS_IMAGE: 2
};

let image_resources = {
    images: [undefined, undefined, undefined],
    image_loaded_flags: [false, false, false]
};

class DrawMatch {
    constructor(canvas, match, bot1_name, bot2_name) {
        this.data = initialData;
        
        this.data.canvas = canvas;
        this.data.match = match;
        this.data.bot1_name = bot1_name;
        this.data.bot2_name = bot2_name;
    }
    
    start() {
        this._startLoadingImages();
        this.data.animation_timer = window.setInterval(this._drawFrame.bind(this), 100);
    }
    
    stop() {
        window.clearInterval(this.data.animation_timer);
    }
    
    _drawFrame() {
        let context = this.data.canvas.getContext("2d");
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
        context.fillStyle = "green";
        context.fillRect(10, 10, 100, 100);
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
}

export default DrawMatch;
