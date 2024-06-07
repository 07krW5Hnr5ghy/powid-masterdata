package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ProvinceDTO;
import com.proyect.masterdata.dto.request.RequestProvince;
import com.proyect.masterdata.dto.request.RequestProvinceSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.ProvinceMapper;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.ProvinceRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IProvince;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import java.util.Date;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class ProvinceImpl implements IProvince {

    private final ProvinceRepository provinceRepository;
    private final ProvinceRepositoryCustom provinceRepositoryCustom;
    private final DepartmentRepository departmentRepository;
    private final ProvinceMapper provinceMapper;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String username, String department)
            throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        boolean existsProvince;
        Department departmentData;
        try {
            user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            existsProvince = provinceRepository.existsByName(name.toUpperCase());
            departmentData = departmentRepository.findByNameAndStatusTrue(department.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user==null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (existsProvince) {
            throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
        }

        if (departmentData == null) {
            throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
        }

        try {
            Province newProvince = provinceRepository.save(Province.builder()
                    .name(name.toUpperCase())
                    .department(departmentData)
                    .departmentId(departmentData.getId())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_PROVINCE","ADD PROVINCE "+newProvince.getName()+".",user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String username, String department) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            boolean existsProvince;
            Department departmentData;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                existsProvince = provinceRepository.existsByName(name.toUpperCase());
                departmentData = departmentRepository.findByNameAndStatusTrue(department.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (existsProvince) {
                throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
            }

            if (departmentData == null) {
                throw new BadRequestExceptions(Constants.ErrorDepartment.toUpperCase());
            }

            try {
                Province newProvince = provinceRepository.save(Province.builder()
                        .name(name.toUpperCase())
                        .department(departmentData)
                        .departmentId(departmentData.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_PROVINCE","ADD PROVINCE "+newProvince.getName()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Province province;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                province = provinceRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (province == null) {
                throw new BadRequestExceptions(Constants.ErrorProvince.toUpperCase());
            }

            try {
                province.setRegistrationDate(new Date(System.currentTimeMillis()));
                province.setStatus(false);
                province.setTokenUser(user.getUsername());
                provinceMapper.provinceToProvinceDTO(provinceRepository.save(province));
                iAudit.save("DELETE_PROVINCE","DELETE PROVINCE "+province.getName()+".",user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Province province;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                province = provinceRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (province == null) {
                throw new BadRequestExceptions(Constants.ErrorProvince.toUpperCase());
            }

            try {
                province.setRegistrationDate(new Date(System.currentTimeMillis()));
                province.setStatus(true);
                province.setTokenUser(user.getUsername());
                provinceMapper.provinceToProvinceDTO(provinceRepository.save(province));
                iAudit.save("ACTIVATE_PROVINCE","ACTIVATE PROVINCE "+province.getName()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
            }
        });
    }

    @Override
    public CompletableFuture<List<ProvinceDTO>> listProvince() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                return provinceMapper.listProvinceToListProvinceDTO(provinceRepository.findAllByStatusTrue());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ProvinceDTO>> list(String name, String user, Long codeDepartment, String nameDepartment, String sort,
            String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Province> provincePage;
            try {
                provincePage = provinceRepositoryCustom.searchForProvince(name, user, codeDepartment, nameDepartment, sort,
                        sortColumn, pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (provincePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(provinceMapper.listProvinceToListProvinceDTO(provincePage.getContent()),
                    provincePage.getPageable(), provincePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ProvinceDTO>> listStatusFalse(String name, String user, Long codeDepartment, String nameDepartment,
            String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Province> provincePage;
            try {
                provincePage = provinceRepositoryCustom.searchForProvince(name, user, codeDepartment, nameDepartment, sort,
                        sortColumn, pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (provincePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(provinceMapper.listProvinceToListProvinceDTO(provincePage.getContent()),
                    provincePage.getPageable(), provincePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ProvinceDTO>> listProvinceByDepartment(String department)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Department departmentData;
            List<Province> provinces;

            try {
                departmentData = departmentRepository.findByNameAndStatusTrue(department.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (departmentData == null) {
                throw new BadRequestExceptions(Constants.ErrorDepartment);
            }

            try {
                provinces = provinceRepository.findAllByDepartmentIdAndStatusTrue(departmentData.getId());
                return provinceMapper.listProvinceToListProvinceDTO(provinces);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
