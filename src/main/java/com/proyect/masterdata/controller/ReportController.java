package com.proyect.masterdata.controller;

import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IReport;
import com.proyect.masterdata.services.IUtil;
import lombok.AllArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.io.ByteArrayInputStream;
import java.time.OffsetDateTime;
import java.util.Date;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("report")
@AllArgsConstructor
public class ReportController {
    private final IReport iReport;
    private final IUtil iUtil;
    @GetMapping("general-stock")
    private ResponseEntity<byte[]> general(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iReport.generalStockReport(user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=inventario_general.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("warehouse-stock")
    private ResponseEntity<byte[]> warehouseStock(
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        CompletableFuture<ByteArrayInputStream> result = iReport.warehouseStockReport(user);
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=inventario_almacen.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("daily-sale")
    private ResponseEntity<byte[]> dailySale(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<ByteArrayInputStream> result = iReport.dailySalesSummary(
                registrationStartDate,
                registrationEndDate,
                user
        );
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=resumen.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("seller")
    private ResponseEntity<byte[]> salesSeller(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<ByteArrayInputStream> result = iReport.salesBySellerSummary(
                registrationStartDate,
                registrationEndDate,
                user
        );
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=venta_vendedor.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("brand")
    private ResponseEntity<byte[]> salesBrand(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<ByteArrayInputStream> result = iReport.salesByBrandSummary(
                registrationStartDate,
                registrationEndDate,
                user
        );
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=venta_marca.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("brand/daily")
    private ResponseEntity<byte[]> salesBrandDaily(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<ByteArrayInputStream> result = iReport.dailySalesByBrandSummary(
                registrationStartDate,
                registrationEndDate,
                user
        );
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=venta_marca_diario.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("status")
    private ResponseEntity<byte[]> salesStatus(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<ByteArrayInputStream> result = iReport.salesByStatusSummary(
                registrationStartDate,
                registrationEndDate,
                user
        );
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=venta_status.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }

    @GetMapping("category")
    private ResponseEntity<byte[]> salesCategory(
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam("user") String user
    ) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        CompletableFuture<ByteArrayInputStream> result = iReport.salesByCategory(
                registrationStartDate,
                registrationEndDate,
                user
        );
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Disposition", "attachment; filename=venta_categoria.xlsx");
        return ResponseEntity.ok()
                .headers(headers)
                .contentType(MediaType.APPLICATION_OCTET_STREAM)
                .body(result.get().readAllBytes());
    }
}
