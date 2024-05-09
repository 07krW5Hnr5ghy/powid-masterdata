package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ISupplierType;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("supplier-type")
@AllArgsConstructor
public class SupplierTypeController {
    private final ISupplierType iSupplierType;
    @PostMapping()
    private ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("tokenUser") String tokenUser
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iSupplierType.saveAsync(name,tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping()
    private ResponseEntity<List<String>> listSupplierTypes() throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<String>> result = iSupplierType.listSupplierType();
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
