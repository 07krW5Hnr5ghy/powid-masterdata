package com.proyect.masterdata.controller;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ITemplate;
import lombok.AllArgsConstructor;
import org.apache.http.Header;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.ByteArrayInputStream;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("template")
@AllArgsConstructor
public class TemplateController {
    private final ITemplate iTemplate;

    @GetMapping("shipment")
    private ResponseEntity<byte[]> shipment(
            @RequestParam("quantity") Integer quantity,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iTemplate.shipment(quantity,user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=embarque.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("stock-transfer")
    private ResponseEntity<byte[]> stockTransfer(
            @RequestParam("quantity") Integer quantity,
            @RequestParam("warehouse") String warehouse,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iTemplate.stockTransfer(quantity,warehouse,user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=transferencia.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("stock-return")
    private ResponseEntity<byte[]> stockReturn(
            @RequestParam("quantity") Integer quantity,
            @RequestParam("purchaseSerial") String purchaseSerial,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iTemplate.stockReturn(quantity,purchaseSerial,user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=devolucion_inventario.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("stock-replenishment")
    private ResponseEntity<byte[]> stockReplenishment(
            @RequestParam("orderId") Long orderId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iTemplate.stockReplenishment(orderId,user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=reposicion_inventario.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("order-stock")
    private ResponseEntity<byte[]> orderStock(
            @RequestParam("orderId") Long orderId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iTemplate.orderStock(orderId,user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=preparacion_pedido.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("order-return")
    private ResponseEntity<byte[]> orderReturn(
            @RequestParam("orderId") Long orderId,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iTemplate.orderReturn(orderId,user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=devolucion_pedido.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("product")
    private ResponseEntity<byte[]> product(
            @RequestParam("quantity") Integer quantity,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iTemplate.product(quantity,user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=productos.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }
}
