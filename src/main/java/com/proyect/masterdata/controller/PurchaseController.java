package com.proyect.masterdata.controller;

import java.util.List;

import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.services.IPurchase;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IPurchaseItem;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("purchase")
@AllArgsConstructor
public class PurchaseController {

    private final IPurchase iPurchase;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    //@PreAuthorize("hasAuthority('ROLE:STOCK') and hasAuthority('ACCESS:PURCHASE_POST')")
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("serial") String serial,
            @RequestBody() List<RequestPurchaseItem> purchaseItemList,
            @RequestParam("supplierRuc") String supplierRuc,
            @RequestParam("documentName") String documentName,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iPurchase.save(serial, supplierRuc, documentName, purchaseItemList, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping("pagination")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<Page<PurchaseDTO>> list(
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "documentName", required = false) String documentName,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize
    ) throws BadRequestExceptions {
        Page<PurchaseDTO> result = iPurchase.list(serial,user,documentName,sort,sortColumn,pageNumber,pageSize);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping()
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<List<PurchaseDTO>> listPurchase(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<PurchaseDTO> result = iPurchase.listPurchase(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

    @GetMapping("status-false")
    //@PreAuthorize("hasAnyAuthority('ROLE:STOCK','ROLE:BUSINESS','ROLE:ADMINISTRATION') and hasAuthority('ACCESS:PURCHASE_GET')")
    public ResponseEntity<List<PurchaseDTO>> listPurchaseFalse(
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        List<PurchaseDTO> result = iPurchase.listPurchaseFalse(user);
        return new ResponseEntity<>(result,HttpStatus.OK);
    }

}
