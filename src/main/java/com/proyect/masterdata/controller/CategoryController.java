package com.proyect.masterdata.controller;

import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestCreateCategory;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.services.ICategory;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@CrossOrigin({"*"})
@RequestMapping("/category")
@AllArgsConstructor
public class CategoryController {
    private final ICategory iCategory;
    @PostMapping()
    public ResponseEntity<ResponseSuccess> save(
            @RequestParam("name") String name,
            @RequestParam("description") String description,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iCategory.save(name,description,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PostMapping(value = "/categories")
    public ResponseEntity<ResponseSuccess> saveall(
            @RequestBody() List<RequestCreateCategory> categories,
            @RequestParam("user") String user
    ) throws BadRequestExceptions {
        ResponseSuccess result = iCategory.saveAll(categories,user);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @PutMapping()
    public ResponseEntity<CategoryDTO> update(
            @RequestBody() RequestCategory requestCategory
    ) throws BadRequestExceptions {
        CategoryDTO result = iCategory.update(requestCategory);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping()
    public ResponseEntity<ResponseDelete> delete(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        ResponseDelete result = iCategory.delete(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @DeleteMapping(value = "/categories")
    public ResponseEntity<ResponseDelete> deleteall(
            @RequestBody() List<Long> codes
    ) throws BadRequestExceptions {
        ResponseDelete result = iCategory.deleteAll(codes);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping()
    public ResponseEntity<List<CategoryDTO>> list() throws BadRequestExceptions {
        List<CategoryDTO> result = iCategory.list();
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/code")
    public ResponseEntity<CategoryDTO> findByCode(
            @RequestParam("code") Long code
    ) throws BadRequestExceptions {
        CategoryDTO result = iCategory.findByCode(code);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @GetMapping(value = "/name")
    public ResponseEntity<CategoryDTO> findByName(
            @RequestParam("name") String name
    ) throws BadRequestExceptions {
        CategoryDTO result = iCategory.findByName(name);
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
