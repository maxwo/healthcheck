/**
 * 
 */
package io.github.maxwo.healthcheck.dao.impl;

import org.springframework.stereotype.Component;

import io.github.maxwo.healthcheck.dao.HealthCheckDao;

/**
 * @author max
 *
 */
@Component
public class HealthCheckDaoImpl implements HealthCheckDao {
	
	private boolean state = false;

	/* (non-Javadoc)
	 * @see io.github.maxwo.healthcheck.dao.HealthCheckDao#setState(boolean)
	 */
	@Override
	public void setState(boolean state) {
		this.state = state;
	}

	/* (non-Javadoc)
	 * @see io.github.maxwo.healthcheck.dao.HealthCheckDao#getState()
	 */
	@Override
	public boolean getState() {
		return state;
	}

}
