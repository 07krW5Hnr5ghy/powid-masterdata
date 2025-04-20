package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.KardexInputDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IKardexInput;
import com.proyect.masterdata.services.IUtil;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("kardex-input")
@AllArgsConstructor
public class KardexInputController {
    private final IKardexInput iKardexInput;
    private final IUtil iUtil;
    @GetMapping()
    public ResponseEntity<Page<KardexInputDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "quantity",required = false) Integer quantity,
            @RequestParam(value = "lotNumber",required = false) Long lotNumber,
            @RequestParam(value = "product",required = false) String product,
            @RequestParam(value = "productId",required = false) UUID productId,
            @RequestParam(value = "username",required = false) String username,
            @RequestParam(value = "warehouse",required = false) String warehouse,
            @RequestParam(value = "unitPrice",required = false) Double unitPrice,
            @RequestParam(value = "registrationStartDate",required = false) String rStartDate,
            @RequestParam(value = "registrationEndDate",required = false) String rEndDate,
            @RequestParam(value = "updateStartDate",required = false) String uStartDate,
            @RequestParam(value = "updateEndDate",required = false) String uEndDate,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions, ExecutionException, InterruptedException {
        OffsetDateTime registrationStartDate = iUtil.parseToOffsetDateTime(rStartDate,true);
        OffsetDateTime registrationEndDate = iUtil.parseToOffsetDateTime(rEndDate, false);
        OffsetDateTime updateStartDate = iUtil.parseToOffsetDateTime(uStartDate,true);
        OffsetDateTime updateEndDate = iUtil.parseToOffsetDateTime(uEndDate,false);
        CompletableFuture<Page<KardexInputDTO>> result = iKardexInput.list(
                user,
                quantity,
                lotNumber,
                product,
                productId,
                username,
                warehouse,
                unitPrice,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                sort,
                sortColumn,
                pageNumber,
                pageSize);
        return new ResponseEntity<>(result.get(), HttpStatus.OK);
    }
}
