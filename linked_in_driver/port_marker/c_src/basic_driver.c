
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

#include <erl_driver.h>
#include <ei.h>


#include <wand/MagickWand.h>

typedef struct _basic_drv_t {
  ErlDrvPort port;
} basic_drv_t;

static ErlDrvData
drv_start(ErlDrvPort port, char *buff __attribute__((unused)));

static void
drv_stop(ErlDrvData handle);

static void
drv_outputv(ErlDrvData handle, ErlIOVec *ev);

static ErlDrvEntry basic_driver_entry = {
    NULL,                             /* init */
    start,                            /* startup */ #1
    stop,                             /* shutdown */ #2
    NULL,                             /* output */
    NULL,                             /* ready_input */
    NULL,                             /* ready_output */
    "basic_drv",                      /* the name of the driver */
    NULL,                             /* finish */
    NULL,                             /* handle */
    NULL,                             /* control */
    NULL,                             /* timeout */
    drv_outputv,                          /* process */ #3
    NULL,                             /* ready_async */
    NULL,                             /* flush */
    NULL,                             /* call */
    NULL,                             /* event */
    ERL_DRV_EXTENDED_MARKER,          /* ERL_DRV_EXTENDED_MARKER */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MAJOR_VERSION */
    ERL_DRV_EXTENDED_MAJOR_VERSION,   /* ERL_DRV_EXTENDED_MINOR_VERSION */
    ERL_DRV_FLAG_USE_PORT_LOCKING     /* ERL_DRV_FLAGs */
};


/**
 * Function: drv_start
 *
 * Starts up an instance of this Erlang Driver
 *
 * Arguments:
 *
 *  port - The representation of the Erlang port
 *  buff -
 *
 * Returns:
 *
 *  This drivers state
 */
static ErlDrvData
drv_start(ErlDrvPort port, char *buff __attribute__((unused)))
{
    struct port_state *drv = NULL;

    drv = (struct port_state *) driver_alloc(sizeof(struct port_state));

    if (drv == NULL) {
      return ERL_DRV_ERROR_GENERAL;
    }
    drv->port = port;
    return (ErlDrvData)drv;
}

/**
 * Function: drv_stop
 *
 *  Stop the driver from running. In this case there
 *  isn't any real concurrent activity so all we really
 *  do is free our handle.
 *
 * Arguments:
 *
 *  handle - The state data for this driver
 *
 */
static void
drv_stop(ErlDrvData handle)
{
	driver_free((struct port_state *)handle);
}



static inline void
format_error(ei_x_buff *x_buff, const char *err_type, const char *error_msg)
{

	ei_x_encode_tuple_header(x_buff, 3);
	ei_x_encode_atom(x_buff, "error");
	ei_x_encode_atom(x_buff, err_type);
	ei_x_encode_string(x_buff, error_msg);
}




void
mark_image(const unsigned char *img,
	   size_t size,
	   unsigned char ** out_img,
	   size_t *out_size)
{
	MagickWandGenesis();

	/*
// Text effect 1 - shadow effect using MagickShadowImage
// This is derived from Anthony's Soft Shadow effect
// convert -size 300x100 xc:none -font Candice -pointsize 72 \
//           -fill white  -stroke black  -annotate +25+65 'Anthony' \
//           \( +clone -background navy  -shadow 70x4+5+5 \) +swap \
//           -background lightblue -flatten  -trim +repage  font_shadow_soft.jpg

//NOTE - if an image has a transparent background, adding a border of any colour other
// than "none" will remove all the transparency and replace it with the border's colour
*/
	MagickWand *magick_wand = NULL;
	MagickWand *c_wand = NULL;
	DrawingWand *d_wand = NULL;
	PixelWand *p_wand = NULL;

	magick_wand = NewMagickWand();
	d_wand = NewDrawingWand();
	p_wand = NewPixelWand();
	PixelSetColor(p_wand,"none");
	// Create a new transparent image
	int status=MagickReadImageBlob(magick_wand, img, size);
	if (status == MagickFalse)
		ThrowWandException(magick_wand);

	// Set up a 72 point white font
	PixelSetColor(p_wand,"white");
	DrawSetFillColor(d_wand,p_wand);
	DrawSetFont (d_wand, "Verdana-Bold-Italic" ) ;
	DrawSetFontSize(d_wand,72);
	// Add a black outline to the text
	PixelSetColor(p_wand,"black");
	DrawSetStrokeColor(d_wand,p_wand);
	// Turn antialias on - not sure this makes a difference
	DrawSetTextAntialias(d_wand,MagickTrue);
	// Now draw the text
	DrawAnnotation(d_wand,25,65, (unsigned char *) "Stock Image");
	// Draw the image on to the magick_wand
	MagickDrawImage(magick_wand,d_wand);

	// Trim the image down to include only the text
	MagickTrimImage(magick_wand,0);

	// equivalent to the command line +repage
	MagickResetImagePage(magick_wand,"");

	// Make a copy of the text image
	c_wand = CloneMagickWand(magick_wand);

	// Set the background colour to blue for the shadow
	PixelSetColor(p_wand,"blue");

	MagickSetImageBackgroundColor(magick_wand,p_wand);
	// Opacity is a real number indicating (apparently) percentage
	MagickShadowImage(magick_wand,70,4,5,5);

	// Composite the text on top of the shadow
	MagickCompositeImage(magick_wand,c_wand,OverCompositeOp,5,5);

	if(c_wand)c_wand = DestroyMagickWand(c_wand);
	c_wand = NewMagickWand();

	// Create a new image the same size as the text image and put a solid colour
	// as its background
	PixelSetColor(p_wand,"rgb(0,0,255)");
	MagickNewImage(c_wand,MagickGetImageWidth(magick_wand),
		       MagickGetImageHeight(magick_wand),p_wand);
	// Now composite the shadowed text over the plain background
	MagickCompositeImage(c_wand,magick_wand,OverCompositeOp,0,0);
	// and write the result
	*out_img = MagickGetImageBlob(magick_wand, out_size);

	/* Clean up */
	if(magick_wand)magick_wand = DestroyMagickWand(magick_wand);
	if(c_wand)c_wand = DestroyMagickWand(c_wand);
	if(d_wand)d_wand = DestroyDrawingWand(d_wand);
	if(p_wand)p_wand = DestroyPixelWand(p_wand);

	MagickWandTerminus();

}



static void
drv_outputv(ErlDrvData handle, ErlIOVec *ev)
{
	struct port_state* d = (struct port_state *) handle;
	ErlDrvBinary *buff_bin = ev->binv[0];
	char *buff = buff_bin->orig_bytes;
	char atom[MAXATOMLEN];

	void * data = NULL;
	void * udata = NULL;

	ei_x_buff x_buff;
	ei_x_new_with_version(&x_buff);

	// The decoding index
	int index = 0;

	int arity;
	if ( ei_decode_tuple_header(buff, &index, &arity)) {
		format_error(&x_buff, "must_pass_tuple", "must pass tuple");
		goto cleanup_out;
	}

	if (arity != 3) {
		format_error(&x_buff, "expecting_arity_3",
			     "Expecting a tuple of arity 3");
		goto cleanup_out;
	}

	erlang_pid pid;
	if (ei_decode_pid(buff, &index, &pid)) {
		format_error(&x_buff, "expected_pid",
			     "Expected pid as the first element"
			     " of the tuple.");
		goto cleanup_out;
	}


	long data_len;
	if (ei_decode_long(buff, &index, &data_len)) {
		format_error(&x_buff, "expecting_third_element_long",
			     "Expecting the length of the next binary");
		goto cleanup_out;
	}


	data = malloc(data_len);
	if (! data) {
		// this is really bad
		format_error(&x_buff,, "unable_to_alloc_data",
			     "Unable to alloc the data");
		goto cleanup_out;

	}

	long real_data_len;
	if (ei_decode_binary(buff, &index, &data, &real_data_len)) {
		format_error(&x_buff,  "unable_to_decode_binary",
			     "Unable to decode the binary data");

		goto cleanup_out;
	}

	if (data_len != real_data_len) {
		format_error(&x_buff, "length_doesnt_match",
			     "The length of the binary doesn't match what "
			     "we expected");
		goto cleanup_out;
	}

	long out_image_size;

	mark_image(data,
		   real_data_len,
		   &udata,
		   &out_image_size);

	ei_x_encode_tuple_header(&x_buff, 3);
	ei_x_encode_pid(&x_buff, &pid);
	ei_x_encode_long(&x_buff, out_image_size);
	ei_x_encode_binary(&x_buff, udata, out_image_size);

	MagickRelinquishMemory(out);


cleanup_out:
	if(x_buff.buff)
		driver_output(d->port, x_buff.buff, x_buff.index);

	ei_x_free(&x_buff);

	if (data)
		free(data);
}
