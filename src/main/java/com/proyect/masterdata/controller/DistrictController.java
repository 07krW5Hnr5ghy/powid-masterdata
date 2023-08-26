package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.IDistrict;
import lombok.AllArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/district")
@AllArgsConstructor
public class DistrictController {
    private final IDistrict iDistrict;

    @PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeDepartment") Long codeDepartment
    ) throws BadRequestExceptions {
        ResponseSuccess result = iDistrict.save(name,user, codeDepartment);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/districts", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestParam("user") String user,
            @RequestParam("codeProvince") Long codeProvince,
            @RequestBody() List<String> names
    ) throws BadRequestExceptions {
        ResponseSuccess result = iDistrict.saveAll(names, user, codeProvince);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DistrictDTO> update(
            @RequestBody() RequestDistrict requestDistrict
    ) throws BadRequestExceptions {
        DistrictDTO result = iDistrict.update(requestDistrict);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseDelete result = iDistrict.delete(code, user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/listdistrict", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<List<DistrictDTO>> listProvince() throws BadRequestExceptions {
        List<DistrictDTO> result = iDistrict.listDistrict();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<DistrictDTO>> list(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeProvince") Long codeProvince,
            @RequestParam("nameProvince") String nameProvince,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<DistrictDTO> result = iDistrict.list(name, user, codeProvince, nameProvince, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value="/statusFalse",consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<Page<DistrictDTO>> listStatusFalse(
            @RequestParam("name") String name,
            @RequestParam("user") String user,
            @RequestParam("codeProvince") Long codeProvince,
            @RequestParam("nameProvince") String nameProvince,
            @RequestParam("sort") String sort,
            @RequestParam("sortColumn") String sortColumn,
            @RequestParam("pageNumber") Integer pageNumber,
            @RequestParam("pageSize") Integer pageSize
    ) throws BadRequestExceptions {
        Page<DistrictDTO> result = iDistrict.listStatusFalse(name, user, codeProvince, nameProvince, sort, sortColumn, pageNumber, pageSize);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<DistrictDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        DistrictDTO result = iDistrict.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
