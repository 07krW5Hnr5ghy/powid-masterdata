package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IProvince;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import java.util.List;

@RestController
@CrossOrigin({ "*" })
@RequestMapping("/province")
@AllArgsConstructor
public class ProvinceController {
    private IProvince iProvince;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("department") String department) throws BadRequestExceptions {
        ResponseSuccess result = iProvince.save(name, user, department);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/provinces", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestParam("user") String user,
            @RequestParam("department") String department,
            @RequestBody() List<String> names) throws BadRequestExceptions {
        ResponseSuccess result = iProvince.saveAll(names, user, department);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProvinceDTO> update(
            @RequestBody() RequestProvince requestProvince) throws BadRequestExceptions {
        ProvinceDTO result = iProvince.update(requestProvince);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user) throws BadRequestExceptions {
        ResponseDelete result = iProvince.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/listprovince", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<ProvinceDTO>> listProvince() throws BadRequestExceptions {
        List<ProvinceDTO> result = iProvince.listProvince();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ProvinceDTO>> list(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeDepartment") Long codeDepartment,
            @RequestParam("nameDepartment") String nameDepartment,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<ProvinceDTO> result = iProvince.list(name, user, codeDepartment, nameDepartment, sort, sortColumn,
                pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/status-false", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<ProvinceDTO>> listStatusFalse(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeDepartment") Long codeDepartment,
            @RequestParam("nameDepartment") String nameDepartment,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize) throws BadRequestExceptions {
        Page<ProvinceDTO> result = iProvince.listStatusFalse(name, user, codeDepartment, nameDepartment, sort,
                sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ProvinceDTO> findByCode(
            @RequestParam("code") Long code) throws BadRequestExceptions {
        ProvinceDTO result = iProvince.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
