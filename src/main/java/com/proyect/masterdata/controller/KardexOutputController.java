package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.KardexOutputDTO;
import com.proyect.masterdata.dto.projections.KardexOutputProjection;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.services.IkardexOutput;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("kardex-output")
@AllArgsConstructor
public class KardexOutputController {
    private final IkardexOutput ikardexOutput;
    private final IUtil iUtil;
    @GetMapping()
    public ResponseEntity<Page<KardexOutputDTO>> list(
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "quantity",required = false) Integer quantity,
            @RequestParam(value = "product",required = false) String product,
            @RequestParam(value = "productId",required = false) UUID productId,
            @RequestParam(value = "username",required = false) String username,
            @RequestParam(value = "warehouse",required = false) String warehouse,
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
        CompletableFuture<Page<KardexOutputDTO>> result = ikardexOutput.list(
                user,
                quantity,
                product,
                productId,
                username,
                warehouse,
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

    @GetMapping("test")
    public List<KardexOutputDTO> selectAll(
            @RequestParam("user") String username,
            @RequestParam("deliveryManifestItemId") UUID deliveryManifestItemId
    ){
        return ikardexOutput.test(username,deliveryManifestItemId);
    }
}
