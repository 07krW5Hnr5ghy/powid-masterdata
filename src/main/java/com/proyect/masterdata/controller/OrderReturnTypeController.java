package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IOrderReturnType;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("order-return-type")
@AllArgsConstructor
public class OrderReturnTypeController {
    private final IOrderReturnType iOrderReturnType;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save (
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderReturnType.save(name,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    public ResponseEntity<List<String>> list() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<String>> result = iOrderReturnType.list();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("status-false")
    public ResponseEntity<List<String>> listFalse() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<String>> result = iOrderReturnType.listFalse();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iOrderReturnType.delete(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @PostMapping("activate")
    public ResponseEntity<ResponseSuccess> activate(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iOrderReturnType.activate(name,tokenUser);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping("filter")
    public ResponseEntity<List<String>> listFilter() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<String>> result = iOrderReturnType.listFilter();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
