package jsky.app.ot.editor.type;

import edu.gemini.spModel.type.SpTypeUtil;

import javax.swing.*;
import java.util.Vector;

/**
 * A ComboBoxModel for use with SP type enums. It hides obsolete types.
 */
@SuppressWarnings( { "unchecked", "serial" })
public class SpTypeComboBoxModel<E extends Enum<E>> extends DefaultComboBoxModel<E> {

    public SpTypeComboBoxModel(Class<E> c) {
        super(new Vector<>(SpTypeUtil.getSelectableItems(c)));
    }

    @Override
    public E getSelectedItem() {
        return (E) super.getSelectedItem();
    }

    @Override
    public E getElementAt(int index) {
        return super.getElementAt(index);
    }

}
