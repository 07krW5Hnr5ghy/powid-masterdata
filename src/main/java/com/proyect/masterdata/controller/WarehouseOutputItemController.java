package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestWarehouseOutput;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IWarehouseOutputItem;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("warehouse-output-item")
@AllArgsConstructor
public class WarehouseOutputItemController {
    private final IWarehouseOutputItem iWarehouseOutputItem;
    @PostMapping()
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:WAREHOUSE_POST')")
    public ResponseEntity<ResponseSuccess> add(
            @RequestBody()RequestWarehouseOutputItem requestWarehouseOutputItem,
            @RequestParam("warehouseOutputId")UUID warehouseOutputId,
            @RequestParam("username") String username
            ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iWarehouseOutputItem.add(requestWarehouseOutputItem,warehouseOutputId,username);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
