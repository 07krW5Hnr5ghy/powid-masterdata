package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DistrictDTO;
import com.proyect.masterdata.dto.request.RequestDistrict;
import com.proyect.masterdata.dto.request.RequestDistrictSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.DistrictMapper;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.DistrictRepositoryCustom;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDistrict;
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
public class DistrictImpl implements IDistrict {

    private final DistrictRepository districtRepository;
    private final DistrictRepositoryCustom districtRepositoryCustom;
    private final ProvinceRepository provinceRepository;
    private final DistrictMapper districtMapper;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String user, String province)
            throws BadRequestExceptions, InternalErrorExceptions {
        boolean existsUser;
        boolean existsDistrict;
        Province provinceData;
        try {
            existsUser = userRepository.existsByUsernameAndStatusTrue(user.toUpperCase());
            existsDistrict = districtRepository.existsByName(name.toUpperCase());
            provinceData = provinceRepository.findByNameAndStatusTrue(province.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsUser) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        if (existsDistrict) {
            throw new BadRequestExceptions(Constants.ErrorDistrictExists.toUpperCase());
        }

        if (provinceData == null) {
            throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
        }

        try {
            District newDistrict = districtRepository.save(District.builder()
                    .name(name.toUpperCase())
                    .tokenUser(user.toUpperCase())
                    .province(provinceData)
                    .provinceId(provinceData.getId())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .build());
            iAudit.save("ADD_DISTRICT","ADD DISTRICT "+newDistrict.getName()+".",user.toUpperCase());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String user, String province) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            boolean existsUser;
            boolean existsDistrict;
            Province provinceData;
            try {
                existsUser = userRepository.existsByUsernameAndStatusTrue(user.toUpperCase());
                existsDistrict = districtRepository.existsByName(name.toUpperCase());
                provinceData = provinceRepository.findByNameAndStatusTrue(province.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (!existsUser) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (existsDistrict) {
                throw new BadRequestExceptions(Constants.ErrorDistrictExists.toUpperCase());
            }

            if (provinceData == null) {
                throw new BadRequestExceptions(Constants.ErrorProvinceExist.toUpperCase());
            }

            try {
                District newDistrict = districtRepository.save(District.builder()
                        .name(name.toUpperCase())
                        .tokenUser(user.toUpperCase())
                        .province(provinceData)
                        .provinceId(provinceData.getId())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .build());
                iAudit.save("ADD_DISTRICT","ADD DISTRICT "+newDistrict.getName()+".",user.toUpperCase());
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
            District district;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                district = districtRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (district == null) {
                throw new BadRequestExceptions(Constants.ErrorDistrict.toUpperCase());
            }

            try {
                district.setStatus(false);
                district.setUpdateDate(new Date(System.currentTimeMillis()));
                district.setTokenUser(user.getUsername());
                districtRepository.save(district);
                iAudit.save("DELETE_DISTRICT","DELETE DISTRICT "+district.getName()+".",user.getUsername());
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
            District district;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                district = districtRepository.findByNameAndStatusFalse(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (district == null) {
                throw new BadRequestExceptions(Constants.ErrorDistrict.toUpperCase());
            }

            try {
                district.setStatus(false);
                district.setUpdateDate(new Date(System.currentTimeMillis()));
                district.setTokenUser(user.getUsername());
                districtRepository.save(district);
                iAudit.save("ACTIVATE_DISTRICT","ACTIVATE DISTRICT "+district.getName()+".",user.getUsername());
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
    public CompletableFuture<List<DistrictDTO>> listDistrict() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAllByStatusTrue());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<DistrictDTO>> list(String name, String user, Long codeProvince, String nameProvince, String sort,
            String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<District> districtPage;
            try {
                districtPage = districtRepositoryCustom.searchForDistrict(name, user, codeProvince, nameProvince, sort,
                        sortColumn, pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (districtPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(districtMapper.listDistrictToListDistrictDTO(districtPage.getContent()),
                    districtPage.getPageable(), districtPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<DistrictDTO>> listStatusFalse(String name, String user, Long codeProvince, String nameProvince,
            String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<District> districtPage;
            try {
                districtPage = districtRepositoryCustom.searchForDistrict(name, user, codeProvince, nameProvince, sort,
                        sortColumn, pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (districtPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(districtMapper.listDistrictToListDistrictDTO(districtPage.getContent()),
                    districtPage.getPageable(), districtPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<DistrictDTO>> listDistrictByProvince(String province)
            throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Province provinceData;
            List<District> districts;

            try {
                provinceData = provinceRepository.findByNameAndStatusTrue(province.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (provinceData == null) {
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }

            try {
                districts = districtRepository.findAllByProvinceIdAndStatusTrue(provinceData.getId());
                return districtMapper.listDistrictToListDistrictDTO(districts);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<DistrictDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try {
                return districtMapper.listDistrictToListDistrictDTO(districtRepository.findAll());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
