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

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseStatus;

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
	value = "/healthcheck",
	tags = {"test", "healthcheck"}
)
@Controller
@RequestMapping(value = "/healthcheck")
public class HealthCheckController {

	/**
	 * Healthy indicator.
	 */
	private boolean healthy = false;

	/**
	 * Indicates whether the service is healthy or not.
	 * 
	 * @return Healthy state.
	 */
	@ApiOperation(value = "Check if the service is healthy.")
	@ApiResponses(value = {
		@ApiResponse(code = 204, message = "Service is healthy."),
		@ApiResponse(code = 500, message = "Service is unhealthy.")
	})
	@CrossOrigin
	@RequestMapping(method = RequestMethod.GET)
	public ResponseEntity<Void> isHealthy() {
		if (healthy) {
			return ResponseEntity.noContent().build();
		} else {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
		}
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
		@ApiResponse(code = 204, message = "Healthy state is set.")
	})
	@CrossOrigin
	@ResponseStatus(HttpStatus.NO_CONTENT)
	@RequestMapping(method = RequestMethod.PUT, consumes = {MediaType.APPLICATION_JSON_VALUE})
	public void setHealty(@RequestBody boolean healthy) {
		this.healthy = healthy;
	}

}
