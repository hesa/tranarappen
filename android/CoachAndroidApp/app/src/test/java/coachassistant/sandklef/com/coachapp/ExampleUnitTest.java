package coachassistant.sandklef.com.coachapp;

import com.sandklef.coachapp.Session.CoachAppSession;
import com.sandklef.coachapp.storage.LocalStorage;

import org.junit.Test;

import static org.junit.Assert.*;

/**
 * To work on unit tests, switch the Test Artifact in the Build Variants view.
 */
public class ExampleUnitTest {
    @Test
    public void addition_isCorrect() throws Exception {
        assertEquals(4, 2 + 2);

        System.out.println("CoachAppSession: " + CoachAppSession.getInstance());
        System.out.println("Local Storage:   " + LocalStorage.getInstance());
        System.out.println("Club:            " + LocalStorage.getInstance().getCurrentClub());


    }
}