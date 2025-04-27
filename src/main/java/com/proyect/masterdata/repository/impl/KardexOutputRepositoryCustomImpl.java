package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.KardexOutputRepositoryCustom;
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
public class KardexOutputRepositoryCustomImpl implements KardexOutputRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<KardexOutput> searchForKardexOutput(
            UUID clientId,
            Integer quantity,
            Long lotNumber,
            Long orderNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            String model,
            String subCategoryProduct,
            String category,
            String size,
            String color,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<KardexOutput> criteriaQuery = criteriaBuilder.createQuery(KardexOutput.class);
        Root<KardexOutput> itemRoot = criteriaQuery.from(KardexOutput.class);
        Join<KardexOutput, Product> kardexOutputProductJoin = itemRoot.join("product");
        Join<KardexOutput, Warehouse> kardexOutputWarehouseJoin = itemRoot.join("warehouse");
        Join<KardexOutput, User> kardexOutputUserJoin = itemRoot.join("user");
        Join<Product,Model> productModelJoin = kardexOutputProductJoin.join("model");
        Join<Product, Size> productSizeJoin = kardexOutputProductJoin.join("size");
        Join<Product, Color> productColorJoin = kardexOutputProductJoin.join("color");
        Join<Product, SubCategoryProduct> productSubCategoryProductJoin = kardexOutputProductJoin.join("subCategoryProduct");
        Join<SubCategoryProduct,CategoryProduct> subCategoryProductCategoryProductJoin = productSubCategoryProductJoin.join("categoryProduct");
        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicate(
                clientId,
                quantity,
                lotNumber,
                orderNumber,
                product,
                productId,
                username,
                warehouse,
                model,
                subCategoryProduct,
                category,
                size,
                color,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                kardexOutputProductJoin,
                kardexOutputUserJoin,
                kardexOutputWarehouseJoin,
                productModelJoin,
                productSizeJoin,
                productColorJoin,
                productSubCategoryProductJoin,
                subCategoryProductCategoryProductJoin);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> kardexOutputList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                kardexOutputList = listASC(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                kardexOutputList = listDESC(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(kardexOutputList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<KardexOutput> orderTypedQuery = entityManager.createQuery(criteriaQuery);
        orderTypedQuery.setFirstResult(pageNumber * pageSize);
        orderTypedQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        Long count = getOrderCount(
                clientId,
                quantity,
                lotNumber,
                orderNumber,
                product,
                productId,
                username,
                warehouse,
                model,
                subCategoryProduct,
                category,
                size,
                color,
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
            Long orderNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            String model,
            String subCategoryProduct,
            String category,
            String size,
            String color,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<KardexOutput> itemRoot,
            Join<KardexOutput,Product> kardexOutputProductJoin,
            Join<KardexOutput,User> kardexOutputUserJoin,
            Join<KardexOutput,Warehouse> kardexOutputWarehouseJoin,
            Join<Product,Model> productModelJoin,
            Join<Product, Size> productSizeJoin,
            Join<Product, Color> productColorJoin,
            Join<Product, SubCategoryProduct> productSubCategoryProductJoin,
            Join<SubCategoryProduct,CategoryProduct> subCategoryProductCategoryProductJoin) {

        List<Predicate> conditions = new ArrayList<>();

        if (clientId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"), clientId)));
        }

        if (quantity != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("quantity"), quantity)));
        }

        if (productId != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("productId"), productId)));
        }

        if(product != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexOutputProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(product != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexOutputProductJoin.get("name")),"%"+product.toUpperCase()+"%"));
        }

        if(warehouse != null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexOutputWarehouseJoin.get("name")),"%"+warehouse.toUpperCase()+"%"));
        }

        if(username!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(kardexOutputUserJoin.get("name")),"%"+username.toUpperCase()+"%"));
        }

        if(model!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productModelJoin.get("name")),"%"+model.toUpperCase()+"%"));
        }

        if(subCategoryProduct!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSubCategoryProductJoin.get("name")),"%"+subCategoryProduct.toUpperCase()+"%"));
        }

        if(category!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(subCategoryProductCategoryProductJoin.get("name")),"%"+category.toUpperCase()+"%"));
        }

        if(size!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productSizeJoin.get("name")),"%"+size.toUpperCase()+"%"));
        }

        if(color!=null){
            conditions.add(criteriaBuilder.like(criteriaBuilder.upper(productColorJoin.get("name")),"%"+color.toUpperCase()+"%"));
        }

        if (lotNumber != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("lotNumber"), lotNumber)));
        }

        if (orderNumber != null) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("orderNumber"), lotNumber)));
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

    private List<Order> listASC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<KardexOutput> itemRoot) {

        List<Order> kardexOutputList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            kardexOutputList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            kardexOutputList.add(criteriaBuilder.asc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            kardexOutputList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            kardexOutputList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            kardexOutputList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            kardexOutputList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return kardexOutputList;

    }

    private List<Order> listDESC(String sortColumn, CriteriaBuilder criteriaBuilder, Root<KardexOutput> itemRoot) {

        List<Order> kardexOutputList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("clientId")) {
            kardexOutputList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if (sortColumn.equalsIgnoreCase("supplierProductId")) {
            kardexOutputList.add(criteriaBuilder.desc(itemRoot.get("supplierProductId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            kardexOutputList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            kardexOutputList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            kardexOutputList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            kardexOutputList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return kardexOutputList;

    }

    private Long getOrderCount(
            UUID clientId,
            Integer quantity,
            Long lotNumber,
            Long orderNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            String model,
            String subCategoryProduct,
            String category,
            String size,
            String color,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate
    ) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<KardexOutput> itemRoot = criteriaQuery.from(KardexOutput.class);
        Join<KardexOutput,Product> kardexOutputProductJoin = itemRoot.join("product");
        Join<KardexOutput,Warehouse> kardexOutputWarehouseJoin = itemRoot.join("warehouse");
        Join<KardexOutput,User> kardexOutputUserJoin = itemRoot.join("user");
        Join<Product,Model> productModelJoin = kardexOutputProductJoin.join("model");
        Join<Product, Size> productSizeJoin = kardexOutputProductJoin.join("size");
        Join<Product, Color> productColorJoin = kardexOutputProductJoin.join("color");
        Join<Product, SubCategoryProduct> productSubCategoryProductJoin = kardexOutputProductJoin.join("subCategoryProduct");
        Join<SubCategoryProduct,CategoryProduct> subCategoryProductCategoryProductJoin = productSubCategoryProductJoin.join("categoryProduct");
        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicate(
                clientId,
                quantity,
                lotNumber,
                orderNumber,
                product,
                productId,
                username,
                warehouse,
                model,
                subCategoryProduct,
                category,
                size,
                color,
                registrationStartDate,
                registrationEndDate,
                updateStartDate,
                updateEndDate,
                criteriaBuilder,
                itemRoot,
                kardexOutputProductJoin,
                kardexOutputUserJoin,
                kardexOutputWarehouseJoin,
                productModelJoin,
                productSizeJoin,
                productColorJoin,
                productSubCategoryProductJoin,
                subCategoryProductCategoryProductJoin);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
