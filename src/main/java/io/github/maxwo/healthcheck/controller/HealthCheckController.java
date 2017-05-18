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
package io.github.maxwo.healthcheck.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import io.github.maxwo.healthcheck.dao.HealthCheckDao;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * Healthcheck controller.
 * 
 * @author Maxime Wojtczak
 */
@Api(
	value = "/healthcheck"
)
@RestController
@RequestMapping(value = "/healthcheck")
public class HealthCheckController {

	/** DAO keeping the healthcheck state */
	@Autowired
	private HealthCheckDao healthCheckDao;

	/**
	 * Retrieve the healthy flag.
	 * 
	 * @return Healthy state.
	 */
	@ApiOperation(value = "Retrieve the healthy flag.")
	@ApiResponses(value = {
		@ApiResponse(code = 200, message = "Health state.", response = Boolean.class),
	})
	@RequestMapping(method = RequestMethod.GET)
	public boolean isHealthy() {
		return healthCheckDao.getState();
	}

	/**
	 * Set the healthy flag.
	 * 
	 * @param healthy Flag value to set.
	 */
	@ApiOperation(
			value = "Sets the healthy state.",
			consumes = MediaType.APPLICATION_JSON_VALUE)
	@ApiResponses(value = {
		@ApiResponse(code = 200, message = "Health state is set.")
	})
	@RequestMapping(method = RequestMethod.PUT, consumes = {MediaType.APPLICATION_JSON_VALUE})
	public void setHealty(@RequestBody final boolean healthy) {
		healthCheckDao.setState(healthy);
	}

}
