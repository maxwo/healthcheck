/**
 * Copyright 2017 Maxime Wojtczak
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the
 * Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
 * INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTIO
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package io.github.maxwo.healthcheck.indicator;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.actuate.health.Health;
import org.springframework.boot.actuate.health.HealthIndicator;
import org.springframework.stereotype.Component;

import io.github.maxwo.healthcheck.dao.HealthCheckDao;

/**
 * Spring health indicator based on health state.
 * 
 * @author Maxime Wojtczak
 */
@Component
public class HealthStateIndicator implements HealthIndicator {

	/** DAO for health state */
	@Autowired
	private HealthCheckDao healthCheckDao;

	/**
	 * {@inheritDoc}
	 * @see org.springframework.boot.actuate.health.HealthIndicator#health()
	 */
	@Override
	public Health health() {
		if (healthCheckDao.getState()) {
			return Health
					.up()
					.withDetail("manualState", "Service status has been manually set to up")
					.build();
		} else {
			return Health
					.down()
					.withDetail("manualState", "Service status has been manually set to down")
					.build();
		}
	}

}
