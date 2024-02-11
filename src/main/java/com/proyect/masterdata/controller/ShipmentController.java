package com.proyect.masterdata.controller;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.proyect.masterdata.dto.ShipmentItemDTO;
import com.proyect.masterdata.dto.request.RequestShipmentItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IShipmentItem;

import lombok.AllArgsConstructor;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("shipment")
@AllArgsConstructor
public class ShipmentController {

    private final IShipmentItem iShipmentItem;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("serial") String serial,
            @RequestParam("warehouse") String warehouse,
            @RequestBody() List<RequestShipmentItem> requestShipmentItemList,
            @RequestParam("tokenUser") String tokenUser) throws BadRequestExceptions {
        ResponseSuccess result = iShipmentItem.save(serial, warehouse, requestShipmentItemList, tokenUser);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<Page<ShipmentItemDTO>> list(
            @RequestParam(value = "serial", required = false) String serial,
            @RequestParam(value = "user", required = true) String user,
            @RequestParam(value = "warehouse", required = false) String warehouse,
            @RequestParam(value = "sort", required = false) String sort,
            @RequestParam(value = "sortColumn", required = false) String sortColumn,
            @RequestParam(value = "pageNumber", required = true) Integer pageNumber,
            @RequestParam(value = "pageSize", required = true) Integer pageSize) throws BadRequestExceptions {
        Page<ShipmentItemDTO> result = iShipmentItem.list(serial, user, warehouse, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

}
