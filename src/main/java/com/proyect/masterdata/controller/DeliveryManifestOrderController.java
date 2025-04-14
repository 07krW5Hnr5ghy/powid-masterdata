package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.request.RequestDeliveryManifestOrder;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestOrderMark;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IDeliveryManifestOrder;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({"*"})
@RequestMapping("delivery-manifest-order")
@AllArgsConstructor
public class DeliveryManifestOrderController {
    private final IDeliveryManifestOrder iDeliveryManifestOrder;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestBody() RequestDeliveryManifestOrder requestDeliveryManifestOrder
    ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        //System.out.println(requestDeliveryManifest);
        CompletableFuture<ResponseSuccess> result = iDeliveryManifestOrder.save(requestDeliveryManifestOrder);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
    @PutMapping()
    public ResponseEntity<ResponseSuccess> markOrders(
            @RequestBody()RequestDeliveryManifestOrderMark requestDeliveryManifestOrderMark
            ) throws BadRequestExceptions, InternalErrorExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ResponseSuccess> result = iDeliveryManifestOrder.markDeliveredOperationsOrders(requestDeliveryManifestOrderMark);
        return new ResponseEntity<>(result.get(),HttpStatus.OK);
    }
}
