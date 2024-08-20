package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CustomerDTO;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICustomer;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("customer")
@AllArgsConstructor
public class CustomerController {
    private final ICustomer iCustomer;
    @PostMapping()
    private ResponseEntity<ResponseSuccess> save(
            @RequestBody()RequestCustomer requestCustomer
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iCustomer.saveAsync(requestCustomer);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("filter")
    private ResponseEntity<List<CustomerDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CustomerDTO>> result = iCustomer.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
