package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestWarehouseOutput;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IWarehouseOutput;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("warehouse-output")
@AllArgsConstructor
public class WarehouseOutputController {
    private final IWarehouseOutput iWarehouseOutput;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestWarehouseOutput requestWarehouseOutput
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iWarehouseOutput.save(requestWarehouseOutput);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @DeleteMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseDelete> close(
            @RequestParam("username") String username,
            @RequestParam("warehouseOutputId") UUID warehouseOutputId
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseDelete> result = iWarehouseOutput.close(username,warehouseOutputId);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseSuccess> reactivate(
            @RequestParam("username") String username,
            @RequestParam("warehouseOutputId") UUID warehouseOutputId
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iWarehouseOutput.reactivate(username,warehouseOutputId);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
