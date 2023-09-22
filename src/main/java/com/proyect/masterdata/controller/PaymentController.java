package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.PaymentDTO;
import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/payment")
@AllArgsConstructor
public class PaymentController {
//    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
//    public ResponseEntity<List<PaymentDTO>> listUsers(
//            @RequestParam("user") String user
//    ) throws BadRequestExceptionsString{
//
//
//        return new ResponseEntity<>(result, HttpStatus.OK);
}
