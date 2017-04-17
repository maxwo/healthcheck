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

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;

/**
 * Unit test for healthcheck.
 */
@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest
@AutoConfigureMockMvc
public class HealthCheckControllerTest {

	@Autowired
	private MockMvc mockMvc;

	@Test
	public void shouldReturn2XXWhenHealthy() throws Exception {
		mockMvc.perform(
			put("/healthcheck")
				.contentType(MediaType.APPLICATION_JSON_VALUE)
				.content(Boolean.TRUE.toString()))
			.andExpect(status().isNoContent());
		
		mockMvc.perform(
			get("/healthcheck"))
			.andExpect(status().isNoContent());
	}

	@Test
	public void shouldReturn500WhenNotHealthy() throws Exception {
		mockMvc.perform(
			put("/healthcheck")
				.contentType(MediaType.APPLICATION_JSON_VALUE)
				.content(Boolean.FALSE.toString()))
			.andExpect(status().isNoContent());
		
		mockMvc.perform(
			get("/healthcheck"))
			.andExpect(status().isInternalServerError());
	}
}
