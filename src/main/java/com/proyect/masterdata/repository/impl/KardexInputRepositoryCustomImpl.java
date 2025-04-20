package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.KardexInput;
import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.repository.KardexInputRepositoryCustom;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Repository
public class KardexInputRepositoryCustomImpl implements KardexInputRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<KardexInput> searchForKardexInput(
            UUID clientId,
            Integer quantity,
            Long lotNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            Double unitPrice,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<KardexInput> criteriaQuery = criteriaBuilder.createQuery(KardexInput.class);
        Root<KardexInput> itemRoot = criteriaQuery.from(KardexInput.class);
        Join<KardexInput, Product> kardexInputProductJoin = itemRoot.join("product");
        Join<KardexInput,Warehouse> kardexInputWarehouseJoin = itemRoot.join("warehouse");
        Join<KardexInput,User> kardexInputUserJoin = itemRoot.join("user");
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
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
                criteriaBuilder,
                itemRoot,
                kardexInputProductJoin,
                kardexInputUserJoin,
                kardexInputWarehouseJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> kardexInputList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                kardexInputList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                kardexInputList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(kardexInputList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<KardexInput> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
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
                updateEndDate
        );

        return new PageImpl<>(orderTypedQuery.getResultList(), pageable, count);
    }
    private List<Predicate> predicate(
            UUID clientId,
            Integer quantity,
            Long lotNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            Double unitPrice,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<KardexInput> itemRoot,
            Join<KardexInput,Product> kardexInputProductJoin,
            Join<KardexInput,User> kardexInputUserJoin,
            Join<KardexInput,Warehouse> kardexInputWarehouseJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (quantity != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("quantity"), quantity)));
        }

        if (lotNumber != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("lotNumber"), lotNumber)));
        }

        if (productId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("productId"), productId)));
        }

        if(product != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexInputProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(product != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexInputProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexInputWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }
        
        if(username!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexInputUserJoin.get("name")),"%"+username.toUpperCase()+"%"));
        }

        if (unitPrice != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("unitPrice"), unitPrice)));
        }
        
        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        return conditions;
    }

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<KardexInput> itemRoot) {

        List<Order> kardexInputList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            kardexInputList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            kardexInputList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            kardexInputList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            kardexInputList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            kardexInputList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            kardexInputList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return kardexInputList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<KardexInput> itemRoot) {

        List<Order> kardexInputList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            kardexInputList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            kardexInputList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            kardexInputList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            kardexInputList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            kardexInputList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            kardexInputList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return kardexInputList;

    }

    private Long getOrderCount(
            UUID clientId,
            Integer quantity,
            Long lotNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            Double unitPrice,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate
    ) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<KardexInput> itemRoot = criteriaQuery.from(KardexInput.class);
        Join<KardexInput,Product> kardexInputProductJoin = itemRoot.join("product");
        Join<KardexInput,Warehouse> kardexInputWarehouseJoin = itemRoot.join("warehouse");
        Join<KardexInput,User> kardexInputUserJoin = itemRoot.join("user");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
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
                criteriaBuilder,
                itemRoot,
                kardexInputProductJoin,
                kardexInputUserJoin,
                kardexInputWarehouseJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
