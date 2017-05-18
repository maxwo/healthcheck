/**
 * 
 */
package io.github.maxwo.healthcheck.dao;

/**
 * @author max
 *
 */
public interface HealthCheckDao {

	void setState(final boolean state);

	boolean getState();

}
