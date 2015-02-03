package edu.gemini.epics.acm;

/**
 * This exception is generated when an inconsistency is detected while
 * monitoring the execution of a command, like another system trying to trigger
 * the same apply record.
 * 
 * @author jluhrs
 *
 */
public class CaCommandPostError extends Exception {

    public CaCommandPostError() {
    }

    public CaCommandPostError(String message) {
        super(message);
    }
}
