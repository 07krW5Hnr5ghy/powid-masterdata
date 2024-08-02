package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.ShipmentDTO;
import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipment;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IShipment;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("shipment")
@AllArgsConstructor
public class ShipmentController {
    private final IShipment iShipment;
    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:SHIPMENT_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestShipment requestShipment,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iShipment.saveAsync(requestShipment, tokenUser);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SHIPMENT_GET')")
    public ResponseEntity<Page<ShipmentDTO>> list(
            @RequestParam(value = "serials", required = false) List<String> serials,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "warehouses", required = false) List<String> warehouses,
            @RequestParam(value = "shipmentTypes", required = false) List<String> shipmentTypes,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<Page<ShipmentDTO>> result = iShipment.list(
                serials,
                user,
                warehouses,
                shipmentTypes,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:ADMINISTRATION','ROLE:BUSINESS') and hasAuthority('ACCESS:SHIPMENT_GET')")
    public ResponseEntity<List<ShipmentDTO>> listShipment(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ShipmentDTO>> result = iShipment.listShipment(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("filter")
    public ResponseEntity<List<ShipmentDTO>> listFilter(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<ShipmentDTO>> result = iShipment.listFilter(user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }

    @GetMapping("check-stock")
    public ResponseEntity<List<CheckStockDTO>> checkStock(
            @RequestParam("user") String user,
            @RequestParam("serial") String serial
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<List<CheckStockDTO>> result = iShipment.checkStock(serial,user);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
