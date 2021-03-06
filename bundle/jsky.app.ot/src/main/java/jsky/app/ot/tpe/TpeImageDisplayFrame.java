package jsky.app.ot.tpe;

import jsky.image.gui.*;

import java.awt.*;

/**
 * Extends NavigatorImageDisplayFrame to add OT/TPE specific features.
 *
 * @version $Revision: 21408 $
 * @author Allan Brighton
 */
public class TpeImageDisplayFrame extends ImageDisplayControlFrame {

    /** Tool bar with Tpe specific commands */
    private TpeToolBar tpeToolBar;

    /**
     * Create a top level window containing an ImageDisplayControl panel.
     *
     * @param fileOrUrl The file name or URL of an image to display.
     */
    public TpeImageDisplayFrame(String fileOrUrl) {
        super(fileOrUrl);
    }

    /** Make and return the menubar */
    @Override
    protected ImageDisplayMenuBar makeMenuBar(DivaMainImageDisplay mainImageDisplay, ImageDisplayToolBar toolBar) {
        return new TpeImageDisplayMenuBar((TpeImageWidget) mainImageDisplay, toolBar);
    }

    /**
     * Make and return the image display control frame.
     *
     * @param size the size (width, height) to use for the pan and zoom windows.
     */
    @Override
    protected ImageDisplayControl makeImageDisplayControl(int size) {
        return new TpeImageDisplayControl(this, size);
    }

    /** Make and return the toolbar */
    @Override
    protected ImageDisplayToolBar makeToolBar(DivaMainImageDisplay mainImageDisplay) {
        // add the Tpe tool bar while we are at it...
        addTpeToolBar();

        // Dragging can cause problems with two tool bars...
        ImageDisplayToolBar toolBar = new TpeImageDisplayToolBar((TpeImageWidget)mainImageDisplay);
        toolBar.setFloatable(false);
        return toolBar;
    }

    /** Add a tool bar for OT/TPE specific operations. */
    protected void addTpeToolBar() {
        tpeToolBar = new TpeToolBar();
        getContentPane().add(tpeToolBar, BorderLayout.WEST);
    }

    /** Return the Tool bar with OT/TPE specific commands */
    protected TpeToolBar getTpeToolBar() {
        return tpeToolBar;
    }

}

